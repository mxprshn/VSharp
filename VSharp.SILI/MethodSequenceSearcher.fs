namespace VSharp.MethodSequences

open System
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
        initialCoreState.model <- Memory.EmptyModel baseMethod (typeModel.CreateEmpty())
        let initialCilState = CilStateOperations.makeInitialState baseMethod initialCoreState
        ILInterpreter.InitFunctionFrame initialCoreState baseMethod None None
        let arguments =
            MethodSequenceHelpers.getThisAndParameterTypes targetMethod |>
                Seq.map MethodSequenceHelpers.createUnknownArgumentOfType |>
                Seq.toList
        let targetMethodCall = Call(targetMethod, None, arguments)
        let initialState =
            {
                cilState = initialCilState
                upcomingSequence = [targetMethodCall]
                currentSequence = List.empty
                id = MethodSequenceHelpers.getNextStateId()
                variableAliases = PersistentDict.empty
            }
        statesToPush.Enqueue(initialState, 1)

    let canHaveDefaultThis (method : Method) =
        not method.HasThis || method.DeclaringType.IsValueType

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
                        | Call(_, _, arguments) :: _ ->
                            let hasHoles = arguments |> List.exists (function Hole _ -> true | _ -> false)
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

    let checkTarget (target : cilState) (state : methodSequenceState) =
        assert(state.cilState.currentLoc.offset = 0<offsets> && targetMethods.Contains state.cilState.currentLoc.method)
        let wlp = Memory.WLP state.cilState.state target.state.pc
        let prevPc = state.cilState.state.pc
        let prevAllocatedTypes = state.cilState.state.allocatedTypes
        state.cilState.state.pc <- wlp
        state.cilState.state.allocatedTypes <- PersistentDict.fold (fun d a b -> PersistentDict.add a b d) state.cilState.state.allocatedTypes target.state.allocatedTypes
        try
            if IsFalsePathCondition state.cilState.state then
                false
            elif IsTruePathCondition state.cilState.state then
                let modelState = Memory.CopyState state.cilState.state
                let currentTypeModel =
                    match target.state.model with
                    | StateModel(_, typeModel, _) -> typeModel
                    | _ -> __unreachable__()

                let mapArgument (arg : methodSequenceArgument) =
                    match arg with
                    | Variable({ typ = typ }) when MethodSequenceHelpers.canBeCreatedBySolver typ ->
                        // If WLP == true, we can just use default values
                        ConcretePrimitive(typ, Activator.CreateInstance typ)
                    | Hole _ -> __unreachable__()
                    | _ -> arg

                let fillHoles (element : methodSequenceElement) =
                    match element with
                    | methodSequenceElement.Call(method, stackKeyOption, methodSequenceArguments) ->
                        let mappedArguments = methodSequenceArguments |> List.map mapArgument
                        methodSequenceElement.Call(method, stackKeyOption, mappedArguments)
                    | methodSequenceElement.CreateDefaultStruct _ -> element

                let filledSequence = state.currentSequence |> List.map fillHoles |> List.rev
                target.state.model <- StateModel(modelState, currentTypeModel, Some filledSequence)
                finishedTargets[target] <- state
                true
            else
                match SolverInteraction.checkSat state.cilState.state with
                | SolverInteraction.SmtSat satInfo ->
                    // How to consider type model?
                    let primitiveModelState =
                        match satInfo.mdl with
                        | StateModel(modelState, _, _) -> modelState
                        | _ -> __unreachable__()

                    let modelState = Memory.CopyState state.cilState.state
                    modelState.model <- StateModel(primitiveModelState, typeModel.CreateEmpty(), None)

                    let currentTypeModel =
                        match target.state.model with
                        | StateModel(_, typeModel, _) -> typeModel
                        | _ -> __unreachable__()

                    let mapArgument (arg : methodSequenceArgument) =
                        match arg with
                        | Variable({ typ = typ; index = index } as id) when MethodSequenceHelpers.canBeCreatedBySolver typ ->
                            let index = if PersistentDict.contains id state.variableAliases then PersistentDict.find state.variableAliases id else index
                            let key = TemporaryLocalVariableKey(typ, index)
                            match Memory.ReadLocalVariable primitiveModelState key with
                            | {term = Concrete(v, t)} -> ConcretePrimitive(t, v)
                            | _ -> __unreachable__()
                        | Hole _ -> __unreachable__()
                        | _ -> arg

                    let fillHoles (element : methodSequenceElement) =
                        match element with
                        | methodSequenceElement.Call(method, stackKeyOption, methodSequenceArguments) ->
                            let mappedArguments = methodSequenceArguments |> List.map mapArgument
                            methodSequenceElement.Call(method, stackKeyOption, mappedArguments)
                        | methodSequenceElement.CreateDefaultStruct _ -> element

                    let filledSequence = state.currentSequence |> List.map fillHoles |> List.rev
                    target.state.model <- StateModel(modelState, currentTypeModel, Some filledSequence)
                    finishedTargets[target] <- state
                    true
                | _ ->
                    false
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
            | Call(nextMethod, _, arguments) :: _ ->
                let hasHoles = arguments |> List.exists (function Hole _ -> true | _ -> false)
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
    let tryConvertSolverModelToSequence (cilState : cilState) =
        let targetMethod = CilStateOperations.entryMethodOf cilState
        if not <| canHaveDefaultThis targetMethod then false
        else
            let modelState, typeModel =
                match cilState.state.model with
                | StateModel(modelState, typeModel, None) -> modelState, typeModel
                | _ -> __unreachable__()
            let readThis() =
                if targetMethod.DeclaringType.IsValueType then
                    let read = (Memory.ReadThis cilState.state targetMethod)
                    match read.term with
                    | Ref(PrimitiveStackLocation key) ->
                        if Memory.IsAllocated modelState key then None
                        else Default targetMethod.DeclaringType |> Some
                    | _ -> __unreachable__()
                else
                    None
            let readArguments() =
                let rec readArgument (remaining : ParameterInfo list) acc =
                    match remaining with
                    | [] -> acc |> List.rev |> Some
                    | currentParameter :: remaining ->
                        if MethodSequenceHelpers.canBeCreatedBySolver currentParameter.ParameterType then
                            let argumentTerm =
                                if currentParameter.ParameterType.IsByRef then
                                    let key = ParameterKey currentParameter
                                    let argumentTerm = Memory.ReadLocalVariable cilState.state key
                                    Memory.Read modelState argumentTerm
                                else
                                    Memory.ReadArgument modelState currentParameter |> cilState.state.model.Complete
                            match argumentTerm with
                                | {term = Concrete(v, _)} ->
                                    let unwrapped = MethodSequenceHelpers.unwrapRefType currentParameter.ParameterType
                                    readArgument remaining (ConcretePrimitive(unwrapped, v) :: acc)
                                | _ -> __unreachable__()
                        else
                            let parameterKey =
                                if currentParameter.ParameterType.IsByRef then
                                    let key = ParameterKey currentParameter
                                    let argumentTerm = Memory.ReadLocalVariable cilState.state key
                                    match argumentTerm.term with
                                    // Is it too hacky?
                                    | Ref(PrimitiveStackLocation key) -> key
                                    | _ -> __unreachable__()
                                else
                                    ParameterKey currentParameter
                            if Memory.IsAllocated modelState parameterKey then
                                None
                            else
                                let unwrapped = MethodSequenceHelpers.unwrapRefType currentParameter.ParameterType
                                readArgument remaining (Default(unwrapped) :: acc)
                readArgument (targetMethod.Parameters |> Seq.toList) []
            if targetMethod.HasThis then
                match readThis() with
                | None -> false
                | Some thisArg ->
                    match readArguments() with
                    | None -> false
                    | Some arguments ->
                        assert(arguments.Length = targetMethod.Parameters.Length)
                        let call = methodSequenceElement.Call(targetMethod, None, thisArg :: arguments)
                        cilState.state.model <- StateModel(modelState, typeModel, Some [call])
                        true
            else
                match readArguments() with
                | None -> false
                | Some arguments ->
                    assert(arguments.Length = targetMethod.Parameters.Length)
                    let call = methodSequenceElement.Call(targetMethod, None, arguments)
                    cilState.state.model <- StateModel(modelState, typeModel, Some [call])
                    true

    let addTarget (parent : cilState option) (target : cilState) =
        match target.state.model with
        | StateModel(_, _, Some _) -> Exists
        | _ ->
            let targetMethod = CilStateOperations.entryMethodOf target
            if not <| isAvailableInPublicApi targetMethod then NotExist
            elif tryConvertSolverModelToSequence target then Exists
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


