namespace VSharp.Interpreter.IL

open System
open System.Reflection
open System.Collections.Generic
open FSharpx.Collections

open VSharp
open VSharp.Concolic
open VSharp.Core
open CilStateOperations
open VSharp.Interpreter.IL
open VSharp.Solver

type public SILI(options : SiliOptions) =

    let timeout = if options.timeout <= 0 then Double.PositiveInfinity else float options.timeout * 1000.0
    let branchReleaseTimeout = if options.timeout <= 0 || not options.releaseBranches then Double.PositiveInfinity else timeout * 80.0 / 100.0

    let mutable branchesReleased = false
    let mutable isStopped = false

    let statistics =
        let dumpInterval = if options.collectContinuousDump then 250 else -1
        new SILIStatistics(dumpInterval)

    let infty = UInt32.MaxValue
    let emptyState = Memory.EmptyState()
    let isConcolicMode =
        match options.executionMode with
        | ConcolicMode -> true
        | SymbolicMode -> false

    let interpreter = ILInterpreter(isConcolicMode)
    let stateInitializer = StateInitializer(isConcolicMode) :> IStateInitializer

    let mutable reportFinished : cilState -> unit = fun _ -> internalfail "reporter not configured!"
    let mutable reportError : cilState -> string -> unit = fun _ -> internalfail "reporter not configured!"
    let mutable reportStateIncomplete : cilState -> unit = fun _ -> internalfail "reporter not configured!"
    let mutable reportIncomplete : InsufficientInformationException -> unit = fun _ -> internalfail "reporter not configured!"
    let mutable reportStateInternalFail : cilState -> Exception -> unit = fun _ -> internalfail "reporter not configured!"
    let mutable reportInternalFail : Method option -> Exception -> unit = fun _ -> internalfail "reporter not configured!"

    let mutable isCoverageAchieved : unit -> bool = always false

    let mutable concolicMachines : Dictionary<cilState, ClientMachine> = Dictionary<cilState, ClientMachine>()

    let () =
        if options.visualize then
            DotVisualizer options.outputDirectory :> IVisualizer |> Application.setVisualizer
        SetMaxBuferSize options.maxBufferSize
        TestGenerator.setMaxBufferSize options.maxBufferSize

    let inCoverageZone coverageZone (entryMethods : Method list) =
        match coverageZone with
        | MethodZone -> fun method -> entryMethods |> List.contains method
        | ClassZone -> fun method -> entryMethods |> List.exists (fun m -> method.DeclaringType.TypeHandle = m.DeclaringType.TypeHandle)
        | ModuleZone -> fun method -> entryMethods |> List.exists (fun m -> method.Module.ModuleHandle = m.Module.ModuleHandle)

    let isSat pc =
        // TODO: consider trivial cases
        emptyState.pc <- pc
        match SolverInteraction.checkSat emptyState with
        | SolverInteraction.SmtSat _
        | SolverInteraction.SmtUnknown _ -> true
        | _ -> false

    let rec mkForwardSearcher = function
        | BFSMode -> BFSSearcher(infty) :> IForwardSearcher
        | DFSMode -> DFSSearcher(infty) :> IForwardSearcher
        | ShortestDistanceBasedMode -> ShortestDistanceBasedSearcher(infty, statistics) :> IForwardSearcher
        | RandomShortestDistanceBasedMode -> RandomShortestDistanceBasedSearcher(infty, statistics) :> IForwardSearcher
        | ContributedCoverageMode -> DFSSortedByContributedCoverageSearcher(infty, statistics) :> IForwardSearcher
        | FairMode baseMode ->
            FairSearcher((fun _ -> mkForwardSearcher baseMode), uint branchReleaseTimeout, statistics) :> IForwardSearcher
        | InterleavedMode(base1, stepCount1, base2, stepCount2) ->
            InterleavedSearcher([mkForwardSearcher base1, stepCount1; mkForwardSearcher base2, stepCount2]) :> IForwardSearcher
        | GuidedMode baseMode ->
            let baseSearcher = mkForwardSearcher baseMode
            GuidedSearcher(infty, options.recThreshold, baseSearcher, StatisticsTargetCalculator(statistics)) :> IForwardSearcher
        | searchMode.ConcolicMode baseMode -> ConcolicSearcher(mkForwardSearcher baseMode) :> IForwardSearcher
        | _ -> __unreachable__()

    let mutable searcher : IBidirectionalSearcher =
        match options.explorationMode with
        | TestCoverageMode(_, searchMode) ->
            if searchMode <> BackwardMode then
                let baseSearcher = mkForwardSearcher searchMode
                let baseSearcher = if isConcolicMode then ConcolicSearcher(baseSearcher) :> IForwardSearcher else baseSearcher
                OnlyForwardSearcher(baseSearcher) :> IBidirectionalSearcher
            else
                OnlyBackwardSearcher(BackwardSearcher(PreviousBasicBlockInitPointSearcher()), TargetedSearcher(infty, always false), stateInitializer) :> IBidirectionalSearcher
        | StackTraceReproductionMode _ -> __notImplemented__()

    let releaseBranches() =
        if not branchesReleased then
            branchesReleased <- true
            statistics.OnBranchesReleased()
            ReleaseBranches()
            let dfsSearcher = DFSSortedByContributedCoverageSearcher(infty, statistics) :> IForwardSearcher
            let dfsSearcher = if isConcolicMode then ConcolicSearcher(dfsSearcher) :> IForwardSearcher else dfsSearcher
            let bidirectionalSearcher = OnlyForwardSearcher(dfsSearcher)
            dfsSearcher.Init <| searcher.States()
            searcher <- bidirectionalSearcher

    let reportState reporter isError cmdArgs cilState message =
        try
            searcher.Remove cilState
            if cilState.history |> Seq.exists (not << statistics.IsBasicBlockCoveredByTest coverageType.ByEntryPointTest)
            then
                let hasException =
                    match cilState.state.exceptionsRegister with
                    | Unhandled _ -> true
                    | _ -> false
                let callStackSize = Memory.CallStackSize cilState.state
                let methodHasByRefParameter (m : Method) = m.Parameters |> Seq.exists (fun pi -> pi.ParameterType.IsByRef)
                let entryMethod = entryMethodOf cilState
                if isError && not hasException
                    then
                        if entryMethod.DeclaringType.IsValueType || methodHasByRefParameter entryMethod
                        then Memory.ForcePopFrames (callStackSize - 2) cilState.state
                        else Memory.ForcePopFrames (callStackSize - 1) cilState.state
                if not isError || statistics.EmitError cilState message
                then
                    match TestGenerator.state2test isError entryMethod cmdArgs cilState message with
                    | Some test ->
                        statistics.TrackFinished cilState
                        reporter test
                        if isCoverageAchieved() then
                            isStopped <- true
                    | None -> ()
        with :? InsufficientInformationException as e ->
            cilState.iie <- Some e
            reportStateIncomplete cilState

    let wrapOnTest (action : Action<UnitTest>) cmdArgs (state : cilState) =
        Logger.info "Result of method %s is %O" (entryMethodOf state).FullName state.Result
        Application.terminateState state
        reportState action.Invoke false cmdArgs state null

    let wrapOnError (action : Action<UnitTest>) cmdArgs (state : cilState) errorMessage =
        if not <| String.IsNullOrWhiteSpace errorMessage then
            Logger.info "Error in %s: %s" (entryMethodOf state).FullName errorMessage
        Application.terminateState state
        reportState action.Invoke true cmdArgs state errorMessage

    let wrapOnStateIIE (action : Action<InsufficientInformationException>) (state : cilState) =
        statistics.IncompleteStates.Add(state)
        Application.terminateState state
        searcher.Remove state
        action.Invoke state.iie.Value

    let wrapOnIIE (action : Action<InsufficientInformationException>) (iie: InsufficientInformationException) =
        action.Invoke iie

    let wrapOnStateInternalFail (action : Action<Method option, Exception>) (state : cilState) (e : Exception) =
        match e with
        | :? InsufficientInformationException as e ->
            if state.iie.IsNone then
                state.iie <- Some e
            reportStateIncomplete state
        | _ ->
            statistics.InternalFails.Add(e)
            Application.terminateState state
            searcher.Remove state
            action.Invoke(state.entryMethod, e)

    let wrapOnInternalFail (action : Action<Method option, Exception>) (method : Method option) (e : Exception) =
        match e with
        | :? InsufficientInformationException as e ->
            reportIncomplete e
        | _ ->
            statistics.InternalFails.Add(e)
            action.Invoke(method, e)

    let coveragePobsForMethod (method : Method) =
        let cfg = method.CFG
        cfg.Sinks |> Seq.map (fun bb ->
            {loc = {offset = bb.FinalOffset; method = method}; lvl = infty; pc = EmptyPathCondition; ipStack = []})
        |> List.ofSeq

    member private x.Forward (s : cilState) =
        let loc = s.currentLoc
        // TODO: update pobs when visiting new methods; use coverageZone
        statistics.TrackStepForward s
        let goodStates, iieStates, errors = interpreter.ExecuteOneInstruction s
        let goodStates, toReportFinished = goodStates |> List.partition (fun s -> isExecutable s || isIsolated s)
        toReportFinished |> List.iter reportFinished
        let errors, toReportExceptions = errors |> List.partition (fun s -> isIsolated s || not <| stoppedByException s)
        let runtimeExceptions, userExceptions = toReportExceptions |> List.partition hasRuntimeException
        runtimeExceptions |> List.iter (fun state -> reportError state null)
        userExceptions |> List.iter reportFinished
        let iieStates, toReportIIE = iieStates |> List.partition isIsolated
        toReportIIE |> List.iter reportStateIncomplete
        let newStates =
            match goodStates with
            | s'::goodStates when LanguagePrimitives.PhysicalEquality s s' -> goodStates @ iieStates @ errors
            | _ ->
                match iieStates with
                | s'::iieStates when LanguagePrimitives.PhysicalEquality s s' -> goodStates @ iieStates @ errors
                | _ ->
                    match errors with
                    | s'::errors when LanguagePrimitives.PhysicalEquality s s' -> goodStates @ iieStates @ errors
                    | _ -> goodStates @ iieStates @ errors
        let concolicMachine : ClientMachine ref = ref null
        if concolicMachines.TryGetValue(s, concolicMachine) then
            let machine = concolicMachine.Value
            let cilState' = machine.StepDone (s::newStates)
            if not <| LanguagePrimitives.PhysicalEquality s cilState' then
                concolicMachines.Remove(s) |> ignore
                concolicMachines.Add(cilState', machine)
        Application.moveState loc s (Seq.cast<_> newStates)
        statistics.TrackFork s newStates
        searcher.UpdateStates s newStates

    member private x.Backward p' s' =
        assert(currentLoc s' = p'.loc)
        let sLvl = levelToUnsignedInt s'.level
        if p'.lvl >= sLvl then
            let lvl = p'.lvl - sLvl
            let pc = Memory.WLP s'.state p'.pc
            match isSat pc with
            | true when not <| isIsolated s' -> searcher.Answer p' (Witnessed s')
            | true ->
                statistics.TrackStepBackward p' s'
                let p = {loc = startingLoc s'; lvl = lvl; pc = pc; ipStack = p'.ipStack}
                Logger.trace "Backward:\nWas: %O\nNow: %O\n\n" p' p
                Application.addGoal p.loc
                searcher.UpdatePobs p' p
            | false ->
                Logger.trace "UNSAT for pob = %O and s'.PC = %s" p' (API.Print.PrintPC s'.state.pc)

    member private x.BidirectionalSymbolicExecution() =
        let mutable action = Stop
        let pick() =
            match searcher.Pick() with
            | Stop -> false
            | a -> action <- a; true
        (* TODO: checking for timeout here is not fine-grained enough (that is, we can work significantly beyond the
                 timeout, but we'll live with it for now. *)
        while not isStopped && pick() && statistics.CurrentExplorationTime.TotalMilliseconds < timeout do
            if statistics.CurrentExplorationTime.TotalMilliseconds >= branchReleaseTimeout then
                releaseBranches()
            match action with
            | GoFront s ->
                try
                    Console.WriteLine "Forward"
                    x.Forward(s)
                with
                | e -> reportStateInternalFail s e
            | GoBack(s, p) ->
                try
                    Console.WriteLine "Backward"
                    x.Backward p s
                with
                | e -> reportStateInternalFail s e
            | Stop -> __unreachable__()

    member private x.AnswerPobs initialStates =
        statistics.ExplorationStarted()

        // For backward compatibility. TODO: remove main pobs at all
        let mainPobs = initialStates |> Seq.collect (fun s -> coveragePobsForMethod (entryMethodOf s))
        Application.spawnStates (Seq.cast<_> initialStates)
        mainPobs |> Seq.map (fun pob -> pob.loc) |> Seq.toArray |> Application.addGoals
        searcher.Init initialStates mainPobs
        initialStates |> Seq.filter isIIEState |> Seq.iter reportStateIncomplete
        match options.executionMode with
        | ConcolicMode ->
            initialStates |> List.iter (fun initialState ->
                let machine = ClientMachine(entryMethodOf initialState, (fun _ -> ()), initialState)
                if not <| machine.Spawn() then
                    internalfail "Unable to spawn concolic machine!"
                concolicMachines.Add(initialState, machine))
            let machine =
                if concolicMachines.Count = 1 then Seq.head concolicMachines.Values
                else __notImplemented'__ "Forking in concolic mode"
            while machine.State.suspended && machine.ExecCommand() do // TODO: make better interaction between concolic and SILI #do
                x.BidirectionalSymbolicExecution()
            // TODO: need to report? #do
//            Logger.error "result state = %O" machine.State
//            reportFinished.Invoke machine.State
        | SymbolicMode ->
            x.BidirectionalSymbolicExecution()
(*        searcher.Statuses() |> Seq.iter (fun (pob, status) ->
            match status with
            | pobStatus.Unknown ->
                Logger.warning "Unknown status for pob at %O" pob.loc
            | _ -> ())*)

    member x.Reset entryMethods =
        API.Reset()
        SolverPool.reset()
        //stateInitializer.Reset()
        statistics.Reset()
        searcher.Reset()
        isStopped <- false
        branchesReleased <- false
        SolverInteraction.setOnSolverStarted statistics.SolverStarted
        SolverInteraction.setOnSolverStopped statistics.SolverStopped
        AcquireBranches()
        isCoverageAchieved <- always false
        match options.explorationMode with
        | TestCoverageMode(coverageZone, _) ->
            Application.setCoverageZone (inCoverageZone coverageZone entryMethods)
            if options.stopOnCoverageAchieved > 0 then
                let checkCoverage() =
                    let cov = statistics.GetApproximateCoverage(entryMethods, coverageType.ByEntryPointTest)
                    cov >= uint options.stopOnCoverageAchieved
                isCoverageAchieved <- checkCoverage
        | StackTraceReproductionMode _ -> __notImplemented__()
        Application.resetMethodStatistics()

    member x.Interpret (isolated : MethodBase seq) (entryPoints : (MethodBase * string[]) seq) (onFinished : Action<UnitTest>)
                       (onException : Action<UnitTest>) (onIIE : Action<InsufficientInformationException>)
                       (onInternalFail : Action<Method option, Exception>) : unit =
        try
            reportInternalFail <- wrapOnInternalFail onInternalFail
            reportStateInternalFail <- wrapOnStateInternalFail onInternalFail
            reportIncomplete <- wrapOnIIE onIIE
            reportStateIncomplete <- wrapOnStateIIE onIIE
            reportFinished <- wrapOnTest onFinished None
            reportError <- wrapOnError onException None
            try
                interpreter.ConfigureErrorReporter reportError
                let initializeIsolatedState methodBase =
                    try
                       stateInitializer.InitializeIsolatedMethodStates methodBase |> Some
                    with e ->
                        reportInternalFail (Application.getMethod methodBase |> Some) e
                        None
                let initializeEntryPointState (methodBase, args) =
                    try
                       stateInitializer.InitializeMainMethodStates methodBase args |> Some
                    with e ->
                        reportInternalFail (Application.getMethod methodBase |> Some) e
                        None
                let statesWithMethods =
                    isolated |> Seq.map initializeIsolatedState
                    |> Seq.append (entryPoints |> Seq.map initializeEntryPointState)
                    |> Seq.choose id
                    |> Seq.toList
                x.Reset(statesWithMethods |> List.map fst)

                let iieStates, initialStates = statesWithMethods |> List.collect snd |> List.partition (fun state -> state.iie.IsSome)
                iieStates |> List.iter reportStateIncomplete
                statistics.SetStatesGetter(fun () -> searcher.States())
                statistics.SetStatesCountGetter(fun () -> searcher.StatesCount)
                if not initialStates.IsEmpty then
                    x.AnswerPobs initialStates
            with
            | e -> reportInternalFail None e
        finally
            statistics.ExplorationFinished()
            API.Restore()
            searcher.Reset()

    member x.Stop() = isStopped <- true

    member x.Statistics with get() = statistics

    interface IDisposable with
        member x.Dispose() = (statistics :> IDisposable).Dispose()
