namespace VSharp.MethodSequences

open System.Collections.Generic
open System.Reflection
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL

type internal methodSequenceResult =
    | NotExist
    | Exists
    | Unknown

type internal IMethodSequenceSearcher =
    abstract Pick : unit -> methodSequenceState option
    abstract Update : methodSequenceState -> methodSequenceState list -> cilState list
    abstract AddTarget : cilState option -> cilState -> methodSequenceResult
    abstract RemoveTarget : cilState -> bool

type internal MethodSequenceSearcher(maxSequenceLength : uint, backwardExplorerFactory : methodSequenceState -> IMethodSequenceBackwardExplorer) =

    let backwardExplorers = Dictionary<methodSequenceState, IMethodSequenceBackwardExplorer>()

    let targetMethods = HashSet<IMethod>()

    let targets = List<cilState>()

    let finishedTargets = Dictionary<cilState, methodSequenceState>()

    // TODO: sort!
    let finishedStates = List<methodSequenceState>()

    // States to go forward in terms of symbolic execution
    // isInMethod == true
    let statesInMethod = Queue<methodSequenceState>()

    // States to pop the next method and start its symbolic execution
    // isInMethod == false; >= 0 elements in sequence; >= 1 methods to call
    let statesToPop = PriorityQueue<methodSequenceState, int>()

    // States to push a new method (i. e. constructor or setter)
    // isInMethod == false; >= 0 elements in sequence; >= 1 methods to call
    let statesToPush = PriorityQueue<methodSequenceState, int>()

    let mutable counter = 0
    let interleaveAt = 10

    let enqueueInitialState (targetMethod : IMethod) =
        let initialCoreState = Memory.EmptyModelState()
        let baseMethod = Application.getMethod Loader.MethodSequenceBase
        initialCoreState.model <- Memory.EmptyModel baseMethod
        let initialCilState = CilStateOperations.makeInitialState baseMethod initialCoreState
        ILInterpreter.InitFunctionFrame initialCoreState baseMethod None None
        let this =
            if MethodSequenceHelpers.hasInstanceThis targetMethod
            then Hole targetMethod.DeclaringType |> Some
            else None
        let arguments =
            targetMethod.Parameters |>
                Seq.map (fun pi -> pi.ParameterType) |>
                Seq.map MethodSequenceHelpers.createUnknownArgumentOfType |>
                Seq.toList
        let targetMethodCall = Call(targetMethod, None, this, arguments)
        let initialState =
            {
                cilState = initialCilState
                upcomingSequence = [targetMethodCall]
                currentSequence = List.empty
                id = MethodSequenceHelpers.getNextStateId()
            }
        statesToPush.Enqueue(initialState, 1)

    let isAvailableInPublicApi (method : Method) =
        method.IsPublic && (method.DeclaringType.IsPublic || method.DeclaringType.IsNestedPublic)

    let canPushOn (state : methodSequenceState) =
        uint (state.currentSequence.Length + state.upcomingSequence.Length) <= maxSequenceLength

    let rec pick() =
        if targets.Count = 0 then None
        else
            Logger.traceWithTag Logger.methodSequenceSearcherTag $"To pop: {statesToPop.Count} To push: {statesToPush.Count} In method: {statesInMethod.Count}"
            counter <- (counter + 1) % interleaveAt
            if statesInMethod.Count = 0 || counter = interleaveAt then
                if statesToPop.Count > 0 then
                    let stateToPop = statesToPop.Dequeue()
                    statesToPush.Enqueue(stateToPop, stateToPop.currentSequence.Length + stateToPop.upcomingSequence.Length)
                    Logger.traceWithTag Logger.methodSequenceSearcherTag $"PICK (TO POP): \n{stateToPop}"
                    Some stateToPop
                elif statesToPush.Count > 0 then
                    let stateToPush = statesToPush.Dequeue()
                    Logger.traceWithTag Logger.methodSequenceSearcherTag $"PICK (TO PUSH): \n{stateToPush}"
                    if not <| backwardExplorers.ContainsKey stateToPush then
                        backwardExplorers[stateToPush] <- backwardExplorerFactory stateToPush
                    match backwardExplorers[stateToPush].MakeNextStep() with
                    | None ->
                        backwardExplorers.Remove stateToPush |> ignore
                        pick()
                    | Some state ->
                        match state.upcomingSequence with
                        | Call(_, _, this, arguments) :: _ ->
                            let hasHoles = MethodSequenceHelpers.thisAndArguments this arguments |> Seq.exists (function Hole _ -> true | _ -> false)
                            if hasHoles then
                                statesToPush.Enqueue(state, state.currentSequence.Length + state.upcomingSequence.Length)
                            else
                                statesToPop.Enqueue(state, 1)
                            statesToPush.Enqueue(stateToPush, stateToPush.currentSequence.Length + stateToPush.upcomingSequence.Length)
                            pick()
                        | _ ->
                            statesToPop.Enqueue(state, 1)
                            statesToPush.Enqueue(stateToPush, stateToPush.currentSequence.Length + stateToPush.upcomingSequence.Length)
                            pick()
                else
                    None
            else
                let stateToForward = statesInMethod.Dequeue()
                Some stateToForward

    let getThisAndArgs (targetMethodElement : methodSequenceElement) =
        match targetMethodElement with
        | Call(method, _, this, args) ->
            let parameters = method.Parameters
            let mapArgument(i, arg) =
                match arg with
                | Variable id ->
                    Some(parameters[i], id)
                | Default _ -> None
                | Hole _ -> __unreachable__()
            let thisId =
                match this with
                | Some this ->
                    match this with
                    | Variable id -> Some id
                    | Default _ -> None
                    | Hole _ -> __unreachable__()
                | _ -> None
            thisId, PersistentDict.ofSeq (args |> Seq.indexed |> Seq.choose mapArgument)
        | CreateDefaultStruct _ -> __unreachable__()

    let checkTarget (target : cilState) (state : methodSequenceState) =
        assert(state.cilState.currentLoc.offset = 0<offsets> && targetMethods.Contains state.cilState.currentLoc.method)
        let wlp = Memory.WLP state.cilState.state target.state.pc
        let prevPc = state.cilState.state.pc
        let prevAllocatedTypes = state.cilState.state.allocatedTypes
        state.cilState.state.pc <- wlp
        state.cilState.state.allocatedTypes <- PersistentDict.fold (fun d a b -> PersistentDict.add a b d) state.cilState.state.allocatedTypes target.state.allocatedTypes
        try
            let checkSat() =
                if IsTruePathCondition state.cilState.state
                then Some <| Memory.CopyState state.cilState.state
                else
                    match SolverInteraction.checkSat state.cilState.state with
                    | SolverInteraction.SmtSat satInfo ->
                        let primitiveModelState =
                            match satInfo.mdl with
                            | StateModel(modelState, _) -> modelState
                            | _ -> __unreachable__()
                        let modelState = Memory.CopyState state.cilState.state
                        modelState.model <- StateModel(primitiveModelState, None)
                        Some modelState
                    | _ -> None

            if IsFalsePathCondition state.cilState.state
            then false
            else
                match checkSat() with
                | Some modelState ->
                    let thisId, argIds = getThisAndArgs state.currentSequence.Head
                    let sequence = List.rev state.currentSequence.Tail
                    target.state.model <- StateModel(modelState, Some { sequence = sequence; this = thisId; args = argIds })
                    finishedTargets[target] <- state
                    true
                | None -> false
        finally
            state.cilState.state.pc <- prevPc
            state.cilState.state.allocatedTypes <- prevAllocatedTypes

    let finish state =
        Logger.traceWithTag Logger.methodSequenceSearcherTag "[Searcher] Finished:"
        Logger.traceWithTag Logger.methodSequenceSearcherTag $"{state}"
        finishedStates.Add state
        // TODO: more effectively?
        let finishedTargets = targets |> Seq.filter (fun tgt -> checkTarget tgt state) |> Seq.toList
        finishedTargets |> List.iter (targets.Remove >> ignore)
        finishedTargets

    let update (parent : methodSequenceState) (newStates : methodSequenceState list) =
        match newStates with
        | [newState] when targetMethods.Contains newState.cilState.currentLoc.method ->
            assert(MethodSequenceHelpers.isInMethod parent && MethodSequenceHelpers.isInMethod newState)
            finish newState
        | [newState] when not <| MethodSequenceHelpers.isInMethod newState ->
            match newState.upcomingSequence with
            | Call(nextMethod, _, this, arguments) :: _ ->
                // TODO: I don't like it, it is the same code like in pick()
                let hasHoles = MethodSequenceHelpers.thisAndArguments this arguments |> Seq.exists (function Hole _ -> true | _ -> false)
                if hasHoles then
                    statesToPush.Enqueue(newState, newState.currentSequence.Length + newState.upcomingSequence.Length)
                elif targetMethods.Contains nextMethod then
                    statesToPop.Enqueue(newState, 0)
                else
                    statesToPop.Enqueue(newState, 1)
            | _ :: _ :: _ -> statesToPop.Enqueue(newState, 1)
            | _ -> __unreachable__()
            []
        | _ :: _ when List.forall MethodSequenceHelpers.isInMethod newStates ->
            List.iter statesInMethod.Enqueue newStates
            []
        | [] ->
            []
        | _ -> __unreachable__()

    // TODO: Consider Nullable<>
    let tryConvertModelToSequence (cilState : cilState) =
        let modelState =
            match cilState.state.model with
            | StateModel(modelState, None) -> modelState
            | _ -> __unreachable__()

        let targetMethod = CilStateOperations.entryMethodOf cilState

        let canConvertThis() =
            let isAllocated() =
                let read = Memory.ReadThis cilState.state targetMethod
                match read.term with
                | Ref(PrimitiveStackLocation key) -> not <| Memory.IsAllocated modelState key
                | _ -> __unreachable__()
            let hasThis = MethodSequenceHelpers.hasInstanceThis targetMethod
            let hasStructThis = MethodSequenceHelpers.isStruct targetMethod.DeclaringType
            not hasThis || (hasStructThis && not <| isAllocated())

        let canConvertArgument (pi : ParameterInfo) =
            let isAllocated() =
                let parameterKey =
                    if pi.ParameterType.IsByRef
                    then
                        let key = ParameterKey pi
                        let argumentTerm = Memory.ReadLocalVariable cilState.state key
                        match argumentTerm.term with
                        // TODO: Is it too hacky?
                        | Ref(PrimitiveStackLocation key) -> key
                        | _ -> __unreachable__()
                    else ParameterKey pi
                Memory.IsAllocated modelState parameterKey
            let isCreatedBySolver = MethodSequenceHelpers.canBeCreatedBySolver pi.ParameterType
            isCreatedBySolver || not <| isAllocated()

        if canConvertThis() && targetMethod.Parameters |> Array.forall canConvertArgument
        then
            cilState.state.model <- StateModel(modelState, Some { sequence = []; this = None; args = PersistentDict.empty })
            true
        else false

    let addTarget (parent : cilState option) (target : cilState) =
        match target.state.model with
        | StateModel(_, Some _) -> Exists
        | _ ->
            let targetMethod = CilStateOperations.entryMethodOf target
            if not <| isAvailableInPublicApi targetMethod then NotExist
            elif tryConvertModelToSequence target then Exists
            else
                if targetMethods.Add targetMethod then
                    enqueueInitialState targetMethod
                    targets.Add target
                    Unknown
                elif targets.Contains target then Unknown
                else
                    let check (cilState : cilState) =
                        if finishedTargets.ContainsKey cilState then
                            checkTarget target finishedTargets[cilState]
                        else false
                    let isFound =
                        match parent with
                        | Some parent ->
                            check parent || (parent <> target && check target)
                        | _ -> check target
                    if not isFound then
                        // TODO: !!!!! Don't check previously checked states (and don't check states which we shouldn't check)
                        if finishedStates |> Seq.exists (checkTarget target) |> not then
                            targets.Add target
                            Unknown
                        else Exists
                    else Exists

    let removeTarget target =
        targets.Remove target

    interface IMethodSequenceSearcher with
        member x.Pick() = pick()
        member x.Update parent newStates = update parent newStates
        member x.AddTarget parent target = addTarget parent target
        member x.RemoveTarget target = removeTarget target
