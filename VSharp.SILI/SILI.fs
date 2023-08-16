namespace VSharp.Interpreter.IL

open System
open System.Collections.Generic
open System.Diagnostics
open System.Reflection
open System.Threading.Tasks
open FSharpx.Collections

open VSharp
open VSharp.Core
open CilStateOperations
open VSharp.Interpreter.IL
open VSharp.MethodSequences
open VSharp.Solver

type public SILI(options : SiliOptions) =

    let isMethodSequenceGenerationEnabled =
        options.methodSequenceStepsShare > 0uy || options.extraMethodSequenceSearchTimeout <> 0

    let hasTimeout = options.timeout > 0
    let timeout =
        if not hasTimeout then Double.PositiveInfinity
        else float options.timeout * 1000.0
    let solverTimeout =
        if options.solverTimeout > 0 then options.solverTimeout * 1000
        // Setting timeout / 2 as solver's timeout doesn't guarantee that SILI
        // stops exactly in timeout. To guarantee that we need to pass timeout
        // based on remaining time to solver dynamically.
        else (max options.timeout options.extraMethodSequenceSearchTimeout) / 2 * 1000
    let branchReleaseTimeout =
        if not hasTimeout then Double.PositiveInfinity
        elif not options.releaseBranches then timeout
        else timeout * 80.0 / 100.0

    let hasStepsLimit = options.stepsLimit > 0u

    do API.ConfigureSolver(SolverPool.mkSolver(solverTimeout))

    let mutable branchesReleased = false
    let mutable isStopped = false

    let statistics = new SILIStatistics(Seq.empty)

    let emptyState = Memory.EmptyState()
    let interpreter = ILInterpreter()

    let methodSequenceExplorer : IMethodSequenceForwardExplorer = MethodSequenceForwardExplorer(interpreter)
    let methodSequenceSearcher : IMethodSequenceSearcher = MethodSequenceSearcher(options.maxMethodSequenceLength, fun s -> MethodSequenceBackwardExplorer s)

    let mutable reportFinished : cilState -> unit = fun _ -> internalfail "reporter not configured!"
    let mutable reportError : cilState -> string -> unit = fun _ -> internalfail "reporter not configured!"
    let mutable reportStateIncomplete : cilState -> unit = fun _ -> internalfail "reporter not configured!"
    let mutable reportIncomplete : InsufficientInformationException -> unit = fun _ -> internalfail "reporter not configured!"
    let mutable reportStateInternalFail : cilState -> Exception -> unit = fun _ -> internalfail "reporter not configured!"
    let mutable reportInternalFail : Method -> Exception -> unit = fun _ -> internalfail "reporter not configured!"
    let mutable reportCrash : Exception -> unit = fun _ -> internalfail "reporter not configured!"

    let mutable isCoverageAchieved : unit -> bool = always false

    let statesWaitingForSequence = Dictionary<cilState, unit -> unit>()

    let () =
        if options.visualize then
            DotVisualizer options.outputDirectory :> IVisualizer |> Application.setVisualizer
        SetMaxBufferSize options.maxBufferSize
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

    let rec mkForwardSearcher mode =
        let getRandomSeedOption() = if options.randomSeed < 0 then None else Some options.randomSeed
        match mode with
        | BFSMode -> BFSSearcher() :> IForwardSearcher
        | DFSMode -> DFSSearcher() :> IForwardSearcher
        | ShortestDistanceBasedMode -> ShortestDistanceBasedSearcher statistics :> IForwardSearcher
        | RandomShortestDistanceBasedMode -> RandomShortestDistanceBasedSearcher(statistics, getRandomSeedOption()) :> IForwardSearcher
        | ContributedCoverageMode -> DFSSortedByContributedCoverageSearcher statistics :> IForwardSearcher
        | ExecutionTreeMode -> ExecutionTreeSearcher(getRandomSeedOption())
        | FairMode baseMode ->
            FairSearcher((fun _ -> mkForwardSearcher baseMode), uint branchReleaseTimeout, statistics) :> IForwardSearcher
        | InterleavedMode(base1, stepCount1, base2, stepCount2) ->
            InterleavedSearcher([mkForwardSearcher base1, stepCount1; mkForwardSearcher base2, stepCount2]) :> IForwardSearcher

    let mutable searcher : IBidirectionalSearcher =
        match options.explorationMode with
        | TestCoverageMode(_, searchMode) ->
            let baseSearcher =
                if options.recThreshold > 0u then
                    GuidedSearcher(mkForwardSearcher searchMode, RecursionBasedTargetManager(statistics, options.recThreshold)) :> IForwardSearcher
                else
                    mkForwardSearcher searchMode
            BidirectionalSearcher(baseSearcher, BackwardSearcher(), DummyTargetedSearcher.DummyTargetedSearcher()) :> IBidirectionalSearcher
        | StackTraceReproductionMode _ -> __notImplemented__()

    let releaseBranches() =
        if not branchesReleased then
            branchesReleased <- true
            statistics.OnBranchesReleased()
            ReleaseBranches()
            let dfsSearcher = DFSSortedByContributedCoverageSearcher statistics :> IForwardSearcher
            let bidirectionalSearcher = OnlyForwardSearcher(dfsSearcher)
            dfsSearcher.Init <| searcher.States()
            searcher <- bidirectionalSearcher

    let printSequence cilState =
        match cilState.state.model with
        | StateModel(_, Some { sequence = sequence; this = this; args = keyMapping }) ->
            Console.WriteLine $"Found sequence for:\n{Print.PrintPC cilState.state.pc}; location: {cilState.currentLoc}"
            let separator = "\n"
            Console.WriteLine $"{sequence |> List.map (fun c -> c.ToString()) |> join separator}"
            Console.WriteLine $"this: {this}; args: {PersistentDict.toSeq keyMapping |> Seq.toList}"
            Console.WriteLine()
        | _ ->
            Console.WriteLine $"No sequence found for PC:\n{Print.PrintPC cilState.state.pc}; location: {cilState.currentLoc}"
            Console.WriteLine()

    let reportState reporter isError cilState message =
        try
            let isNewHistory() =
                let methodHistory = Set.filter (fun h -> h.method.InCoverageZone) cilState.history
                Set.exists (not << statistics.IsBasicBlockCoveredByTest) methodHistory
            let suitableHistory = Set.isEmpty cilState.history || isNewHistory()
            if suitableHistory && not isError || isError && statistics.IsNewError cilState message then
                let callStackSize = Memory.CallStackSize cilState.state
                let methodHasByRefParameter (m : Method) =
                    m.Parameters |> Array.exists (fun pi -> pi.ParameterType.IsByRef)
                let entryMethod = entryMethodOf cilState
                let hasException = isUnhandledError cilState
                if isError && not hasException then
                    if entryMethod.DeclaringType.IsValueType || methodHasByRefParameter entryMethod then
                        Memory.ForcePopFrames (callStackSize - 2) cilState.state
                    else Memory.ForcePopFrames (callStackSize - 1) cilState.state
                statistics.TrackFinished cilState
                let generateTest() =
                    match TestGenerator.state2test isError entryMethod cilState.state message with
                    | Some test ->
                        //statistics.TrackFinished cilState
                        reporter test
                        if isCoverageAchieved() then
                            isStopped <- true
                    | None -> ()
                if not <| hasMethodSequence cilState && isMethodSequenceGenerationEnabled then
                    Console.WriteLine "Enqueue state to report later\n"
                    statesWaitingForSequence.Add(cilState, generateTest)
                else
                    printSequence cilState
                    generateTest()
            else
                        methodSequenceSearcher.RemoveTarget cilState |> ignore
        with :? InsufficientInformationException as e ->
            cilState.iie <- Some e
            reportStateIncomplete cilState

    let wrapOnTest (action : Action<UnitTest>) (state : cilState) =
        let result = Memory.StateResult state.state
        Logger.info "Result of method %s is %O" (entryMethodOf state).FullName result
        Application.terminateState state
        reportState action.Invoke false state null

    let wrapOnError (action : Action<UnitTest>) (state : cilState) errorMessage =
        if not <| String.IsNullOrWhiteSpace errorMessage then
            Logger.info $"Error in {(entryMethodOf state).FullName}: {errorMessage}"
        Application.terminateState state
        reportState action.Invoke true state errorMessage

    let wrapOnStateIIE (action : Action<InsufficientInformationException>) (state : cilState) =
        searcher.Remove state
        statistics.IncompleteStates.Add(state)
        Application.terminateState state
        action.Invoke state.iie.Value

    let wrapOnIIE (action : Action<InsufficientInformationException>) (iie: InsufficientInformationException) =
        action.Invoke iie

    let wrapOnStateInternalFail (action : Action<Method, Exception>) (state : cilState) (e : Exception) =
        match e with
        | :? InsufficientInformationException as e ->
            if state.iie.IsNone then
                state.iie <- Some e
            reportStateIncomplete state
        | _ ->
            searcher.Remove state
            statistics.InternalFails.Add(e)
            Application.terminateState state
            action.Invoke(entryMethodOf state, e)

    let wrapOnInternalFail (action : Action<Method, Exception>) (method : Method) (e : Exception) =
        match e with
        | :? InsufficientInformationException as e ->
            reportIncomplete e
        | _ ->
            statistics.InternalFails.Add(e)
            action.Invoke(method, e)

    let wrapOnCrash (action : Action<Exception>) (e : Exception) = action.Invoke e

    let makeMethodSequenceSearchStep() =
        match methodSequenceSearcher.Pick() with
        | None -> false
        | Some state ->
            let newStates = methodSequenceExplorer.MakeStep state
            methodSequenceSearcher.Update state newStates |> ignore
            true

    let mutable svmStepsCounter = 0

    let makeMethodSequenceSearchStepsIfNeeded =
        let bounded = min options.methodSequenceStepsShare 99uy
        if bounded = 0uy then (fun _ -> ())
        elif bounded >= 50uy then
            let methodSequenceSearcherSteps = float bounded / float (100uy - bounded) |> ceil |> int
            let makeSteps() =
                let mutable counter = 0
                while counter <> methodSequenceSearcherSteps && makeMethodSequenceSearchStep() do
                    counter <- counter + 1
            makeSteps
        else
            let svmSteps = float (100uy - bounded) / float bounded |> ceil |> int
            let makeStep() =
                if svmStepsCounter = svmSteps then
                    svmStepsCounter <- 0
                    makeMethodSequenceSearchStep() |> ignore
                else
                    svmStepsCounter <- svmStepsCounter + 1
            makeStep

    let isTimeoutReached() = hasTimeout && statistics.CurrentExplorationTime.TotalMilliseconds >= timeout
    let shouldReleaseBranches() = options.releaseBranches && statistics.CurrentExplorationTime.TotalMilliseconds >= branchReleaseTimeout
    let isStepsLimitReached() = hasStepsLimit && statistics.StepsCount >= options.stepsLimit

    member x.FindRemainingMethodSequences() =
        if statesWaitingForSequence.Count > 0 then
            if options.extraMethodSequenceSearchTimeout > 0 then
                for state in searcher.States() |> Seq.filter (not << statesWaitingForSequence.ContainsKey) do
                    methodSequenceSearcher.RemoveTarget state |> ignore

                let remainingSvmTimeMs = max 0 ((timeout - statistics.CurrentExplorationTime.TotalMilliseconds) |> ceil |> int)
                let timeoutMs = options.extraMethodSequenceSearchTimeout * 1000 + remainingSvmTimeMs

                let searchForSequences() =
                    let stopwatch = Stopwatch()
                    stopwatch.Start()
                    while stopwatch.ElapsedMilliseconds < timeoutMs && makeMethodSequenceSearchStep() do ()

                let task = Task.Run searchForSequences
                task.Wait(int(float timeoutMs * 1.5)) |> ignore

            for kvp in statesWaitingForSequence do
                let state = kvp.Key
                let generateTest = kvp.Value
                printSequence state
                if not <| hasMethodSequence state then
                    if options.generateTestsWithoutSequence then
                       generateTest()
                    statistics.AddPcWithoutSequence state
                else
                    generateTest()

    static member private AllocateByRefParameters initialState (method : Method) =
        let allocateIfByRef (pi : ParameterInfo) =
            if pi.ParameterType.IsByRef then
                if Memory.CallStackSize initialState = 0 then
                    Memory.NewStackFrame initialState None []
                let typ = pi.ParameterType.GetElementType()
                let position = -pi.Position - 2
                let stackRef = Memory.AllocateTemporaryLocalVariableOfType initialState pi.Name position typ
                Some stackRef
            else
                None
        method.Parameters |> Array.map allocateIfByRef |> Array.toList

    member private x.TrySubstituteTypeParameters (state : state) (methodBase : MethodBase) =
        let method = Application.getMethod methodBase
        let getConcreteType = function
        | ConcreteType t -> Some t
        | _ -> None
        try
            match SolveGenericMethodParameters state.typeStorage method with
            | Some(classParams, methodParams) ->
                let classParams = classParams |> Array.choose getConcreteType
                let methodParams = methodParams |> Array.choose getConcreteType
                if classParams.Length = methodBase.DeclaringType.GetGenericArguments().Length &&
                    (methodBase.IsConstructor || methodParams.Length = methodBase.GetGenericArguments().Length) then
                    let reflectedType = Reflection.concretizeTypeParameters methodBase.ReflectedType classParams
                    let method = Reflection.concretizeMethodParameters reflectedType methodBase methodParams
                    Some method
                else
                    None
            | _ -> None
        with
        | e ->
            reportInternalFail method e
            None

    member private x.FormIsolatedInitialStates (method : Method, initialState : state) =
        try
            initialState.model <- Memory.EmptyModel method
            let cilState = makeInitialState method initialState
            let this(*, isMethodOfStruct*) =
                if method.IsStatic then None // *TODO: use hasThis flag from Reflection
                else
                    let this =
                        if Types.IsValueType method.DeclaringType then
                            Memory.NewStackFrame initialState None []
                            Memory.AllocateTemporaryLocalVariableOfType initialState "this" -1 method.DeclaringType
                        else
                            Memory.MakeSymbolicThis method
                    !!(IsNullReference this) |> AddConstraint initialState
                    Some this
            let parameters = SILI.AllocateByRefParameters initialState method
            Memory.InitFunctionFrame initialState method this (Some parameters)
            let cilStates = ILInterpreter.CheckDisallowNullAttribute method None cilState false id
            assert (List.length cilStates = 1)
            let [cilState] = cilStates
            if isMethodSequenceGenerationEnabled then
                methodSequenceSearcher.AddTarget None cilState |> ignore
            interpreter.InitializeStatics cilState method.DeclaringType List.singleton
        with
        | e ->
            reportInternalFail method e
            []

    member private x.FormEntryPointInitialStates (method : Method, mainArguments : string[], initialState : state) : cilState list =
        try
            assert method.IsStatic
            let hasConcreteMainArguments = mainArguments <> null
            let optionArgs = if not hasConcreteMainArguments then None else Some mainArguments
            let state = { initialState with complete = hasConcreteMainArguments }
            state.model <- Memory.EmptyModel method
            let argsToState args =
                let stringType = typeof<string>
                let argsNumber = MakeNumber mainArguments.Length
                Memory.AllocateConcreteVectorArray state argsNumber stringType args
            let arguments = Option.map (argsToState >> Some >> List.singleton) optionArgs
            Memory.InitFunctionFrame state method None arguments
            if Option.isNone optionArgs then
                // NOTE: if args are symbolic, constraint 'args != null' is added
                let parameters = method.Parameters
                assert(Array.length parameters = 1)
                let argsParameter = Array.head parameters
                let argsParameterTerm = Memory.ReadArgument state argsParameter
                AddConstraint state (!!(IsNullReference argsParameterTerm))
                // Filling model with default args to match PC
                let modelState =
                    match state.model with
                    | StateModel(modelState, _) -> modelState
                    | _ -> __unreachable__()
                let argsForModel = Memory.AllocateVectorArray modelState (MakeNumber 0) typeof<String>
                Memory.WriteStackLocation modelState (ParameterKey argsParameter) argsForModel
            Memory.InitializeStaticMembers state method.DeclaringType
            let initialState = makeInitialState method state
            if isMethodSequenceGenerationEnabled && not hasConcreteMainArguments then
                // TODO: do something with concrete arguments?
                methodSequenceSearcher.AddTarget None initialState |> ignore
            [initialState]
        with
        | e ->
            reportInternalFail method e
            []

    member private x.Forward (s : cilState) =
        makeMethodSequenceSearchStepsIfNeeded()
        let loc = s.currentLoc
        let ip = currentIp s
        // TODO: update pobs when visiting new methods; use coverageZone
        let goodStates, iieStates, errors = interpreter.ExecuteOneInstruction s
        for s in goodStates @ iieStates @ errors do
            if hasRuntimeException s |> not then
                statistics.TrackStepForward s ip
        let goodStates, toReportFinished = goodStates |> List.partition (fun s -> isExecutable s || isIsolated s)
        toReportFinished |> List.iter reportFinished
        let errors, toReportExceptions = errors |> List.partition (fun s -> isIsolated s || not <| stoppedByException s)
        let runtimeExceptions, userExceptions = toReportExceptions |> List.partition hasRuntimeException
        runtimeExceptions |> List.iter (fun state -> reportError state null)
        userExceptions |> List.iter reportFinished
        let iieStates, toReportIIE = iieStates |> List.partition isIsolated
        toReportIIE |> List.iter reportStateIncomplete
        let mutable sIsStopped = false
        let newStates =
            match goodStates, iieStates, errors with
            | s'::goodStates, _, _ when LanguagePrimitives.PhysicalEquality s s' ->
                goodStates @ iieStates @ errors
            | _, s'::iieStates, _ when LanguagePrimitives.PhysicalEquality s s' ->
                goodStates @ iieStates @ errors
            | _, _, s'::errors when LanguagePrimitives.PhysicalEquality s s' ->
                goodStates @ iieStates @ errors
            | _ ->
                sIsStopped <- true
                goodStates @ iieStates @ errors
        Application.moveState loc s (Seq.cast<_> newStates)
        statistics.TrackFork s newStates
        if isMethodSequenceGenerationEnabled && not <| List.isEmpty newStates then
            (s :: newStates) |> Seq.iter (methodSequenceSearcher.AddTarget (Some s) >> ignore)
        searcher.UpdateStates s newStates
        if sIsStopped then
            searcher.Remove s

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
                let p = {loc = startingLoc s'; lvl = lvl; pc = pc}
                Logger.trace $"Backward:\nWas: {p'}\nNow: {p}\n\n"
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
        while not isStopped && not <| isStepsLimitReached() && not <| isTimeoutReached() && pick() do
            if shouldReleaseBranches() then
                releaseBranches()
            match action with
            | GoFront s ->
                try
                    x.Forward(s)
                with
                | e -> reportStateInternalFail s e
            | GoBack(s, p) ->
                try
                    x.Backward p s
                with
                | e -> reportStateInternalFail s e
            | Stop -> __unreachable__()

    member private x.AnswerPobs initialStates =
        statistics.ExplorationStarted()
        // For backward compatibility. TODO: remove main pobs at all
        let mainPobs = []
        Application.spawnStates (Seq.cast<_> initialStates)
        mainPobs |> Seq.map (fun pob -> pob.loc) |> Seq.toArray |> Application.addGoals
        searcher.Init initialStates mainPobs
        initialStates |> Seq.filter isIIEState |> Seq.iter reportStateIncomplete
        x.BidirectionalSymbolicExecution()
        x.FindRemainingMethodSequences()
        searcher.Statuses() |> Seq.iter (fun (pob, status) ->
            match status with
            | pobStatus.Unknown ->
                Logger.warning $"Unknown status for pob at {pob.loc}"
            | _ -> ())

    member x.Reset entryMethods =
        HashMap.clear()
        API.Reset()
        SolverPool.reset()
        statistics.Reset entryMethods
        searcher.Reset()
        isStopped <- false
        branchesReleased <- false
        SolverInteraction.setOnSolverStarted statistics.SolverStarted
        SolverInteraction.setOnSolverStopped statistics.SolverStopped
        AcquireBranches()
        isCoverageAchieved <- always false
        svmStepsCounter <- 0
        statesWaitingForSequence.Clear()
        match options.explorationMode with
        | TestCoverageMode(coverageZone, _) ->
            Application.setCoverageZone (inCoverageZone coverageZone entryMethods)
            (*if options.stopOnCoverageAchieved > 0 then
                let checkCoverage() =
                    let cov = statistics.GetCurrentCoverage entryMethods
                    cov >= uint options.stopOnCoverageAchieved
                isCoverageAchieved <- checkCoverage*)
        | StackTraceReproductionMode _ -> __notImplemented__()

    member x.Interpret (isolated : MethodBase seq) (entryPoints : (MethodBase * string[]) seq) (onFinished : Action<UnitTest>)
                       (onException : Action<UnitTest>) (onIIE : Action<InsufficientInformationException>)
                       (onInternalFail : Action<Method, Exception>) (onCrash : Action<Exception>): unit =
        try
            reportInternalFail <- wrapOnInternalFail onInternalFail
            reportStateInternalFail <- wrapOnStateInternalFail onInternalFail
            reportCrash <- wrapOnCrash onCrash
            reportIncomplete <- wrapOnIIE onIIE
            reportStateIncomplete <- wrapOnStateIIE onIIE
            reportFinished <- wrapOnTest onFinished
            reportError <- wrapOnError onException
            try
                let initializeAndStart () =
                    let trySubstituteTypeParameters method =
                        let emptyState = Memory.EmptyState()
                        (Option.defaultValue method (x.TrySubstituteTypeParameters emptyState method), emptyState)
                    interpreter.ConfigureErrorReporter reportError
                    let isolated =
                        isolated
                        |> Seq.map trySubstituteTypeParameters
                        |> Seq.map (fun (m, s) -> Application.getMethod m, s) |> Seq.toList
                    let entryPoints =
                        entryPoints
                        |> Seq.map (fun (m, a) ->
                            let m, s = trySubstituteTypeParameters m
                            (Application.getMethod m, a, s))
                        |> Seq.toList
                    x.Reset ((isolated |> List.map fst) @ (entryPoints |> List.map (fun (m, _, _) -> m)))
                    let isolatedInitialStates = isolated |> List.collect x.FormIsolatedInitialStates
                    let entryPointsInitialStates = entryPoints |> List.collect x.FormEntryPointInitialStates
                    let iieStates, initialStates =
                        isolatedInitialStates @ entryPointsInitialStates
                        |> List.partition (fun state -> state.iie.IsSome)
                    iieStates |> List.iter reportStateIncomplete
                    statistics.SetStatesGetter(fun () -> searcher.States())
                    statistics.SetStatesCountGetter(fun () -> searcher.StatesCount)
                    if not initialStates.IsEmpty then
                        x.AnswerPobs initialStates
                let explorationTask = Task.Run(initializeAndStart)
                let finished =
                    (*if hasTimeout then explorationTask.Wait(int (timeout * 1.5))
                    else *)explorationTask.Wait(); true
                if not finished then Logger.warning "Execution was cancelled due to timeout"
            with
            | :? AggregateException as e ->
                Logger.warning "Execution was cancelled"
                reportCrash e.InnerException
            | e -> reportCrash e
        finally
            try
                statistics.ExplorationFinished()
                API.Restore()
                searcher.Reset()
            with e -> reportCrash e

    member x.Stop() = isStopped <- true

    member x.Statistics with get() = statistics

    interface IDisposable with
        member x.Dispose() = (statistics :> IDisposable).Dispose()
