namespace VSharp.MethodSequences

open System
open System.Collections.Generic
open System.Reflection
open FSharpx.Collections
open Microsoft.FSharp.Collections
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL

type internal methodSequenceExplorerAction =
    | Forward
    | Pop
    | Push of Method

type internal methodSequenceState =
    {
        cilState : cilState
        isInMethod : bool
        methodsToCall : Method list
        currentSequence : methodSequenceElement list
        locals : PersistentHashMap<Type, int>
        actions : HashSet<methodSequenceExplorerAction>
    }

type internal methodSequenceSearcherResult =
    | NotExist
    | Exists
    | Unknown

type internal IMethodSequenceSearcher =
    abstract Pick : unit -> (methodSequenceExplorerAction * methodSequenceState) list
    abstract Update : methodSequenceState -> methodSequenceState list -> unit
    abstract AddTarget : cilState option -> cilState -> bool

module MethodSequenceHelpers =
    let isPrimitive (t : Type) = t.IsPrimitive || t.IsEnum

type MethodSequenceSearcher(targetMethod : Method, maxSequenceLength : int) =

    let targets = List<cilState>()

    let finishedTargets = Dictionary<cilState, methodSequenceState>()

    // TODO: sort
    let finishedStates = List<methodSequenceState>()

    // States to go forward in terms of symbolic execution
    // isInMethod == true
    let statesInMethod = List<methodSequenceState>()

    // States to push the target method
    // isInMethod == false; > 0 elements in sequence; 0 methods to call
    let statesToFinish = Queue<methodSequenceState>()

    // States to pop the next method and start its symbolic execution
    // isInMethod == false; >= 0 elements in sequence; > 0 methods to call
    let statesToPop = Queue<methodSequenceState>()

    // States to push a new method (i. e. constructor or setter)
    // isInMethod == false; >= 0 elements in sequence; >= 0 methods to call
    let statesToPush = Queue<methodSequenceState>()

    let mutable counter = 0
    let interleaveAt = 10

    do
        let initialCoreState = Memory.EmptyModelState()
        let baseMethod = Application.getMethod Loader.MethodSequenceBase
        initialCoreState.model <- Memory.EmptyModel baseMethod (typeModel.CreateEmpty())
        let initialCilState = CilStateOperations.makeInitialState baseMethod initialCoreState
        ILInterpreter.InitFunctionFrame initialCoreState baseMethod None None
        let initialState =
            {
                cilState = initialCilState
                isInMethod = false
                methodsToCall = List.empty
                currentSequence = List.empty
                locals = PersistentHashMap.empty
                actions = HashSet()
            }
        statesToPush.Enqueue initialState

    let canPushOn (state : methodSequenceState) =
        state.currentSequence.Length + state.methodsToCall.Length <= maxSequenceLength

    let publicFlags =
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        BindingFlags.Public ||| BindingFlags.Instance

    let tryGetThisConstructorToPush (state : methodSequenceState) =
        let nextMethod =
            match state.methodsToCall with
            | [] -> targetMethod
            | nextMethod :: _ -> nextMethod

        if not <| nextMethod.HasThis then
            None
        else
            let declaringType = nextMethod.DeclaringType
            if state.locals.ContainsKey declaringType then
                None
            else
                declaringType.GetConstructors publicFlags |>
                    Seq.sortBy (fun ctor -> ctor.GetParameters().Length) |>
                    Seq.map (Application.getMethod >> Push) |>
                    Seq.tryFind (state.actions.Contains >> not)

    let tryGetConstructorPush (state : methodSequenceState) =
        // TODO: use some smarter logic here
        // In fact we need to look at all methods to call
        let nextMethod =
            match state.methodsToCall with
            | [] -> targetMethod
            | nextMethod :: _ -> nextMethod

        nextMethod.Parameters |>
            Seq.map (fun pi -> pi.ParameterType) |>
            Seq.filter (MethodSequenceHelpers.isPrimitive >> not) |>
            Seq.countBy id |>
            Seq.filter (fun (typ, count) -> not <| state.locals.ContainsKey typ || state.locals[typ] < count) |>
            Seq.collect (fun (typ, _) -> typ.GetConstructors publicFlags) |>
            Seq.sortBy (fun ctor -> ctor.GetParameters().Length) |>
            Seq.map (Application.getMethod >> Push) |>
            Seq.tryFind (state.actions.Contains >> not)

    let tryGetPropertySetterPush (state : methodSequenceState) =
        state.locals.Iterator() |>
            Seq.map fst |>
            Seq.collect (fun typ -> typ.GetProperties publicFlags) |>
            Seq.map (fun pi -> pi.GetSetMethod()) |>
            Seq.filter (fun m -> m <> null) |>
            Seq.map (Application.getMethod >> Push) |>
            Seq.tryFind (not << state.actions.Contains)

    let pick() =
        counter <- (counter + 1) % interleaveAt
        if statesInMethod.Count = 0 || counter = interleaveAt then
            if statesToFinish.Count > 0 then
                let stateToFinish = statesToFinish.Dequeue()
                statesToPush.Enqueue stateToFinish
                let action = Push targetMethod
                [action, stateToFinish]
            elif statesToPop.Count > 0 then
                let stateToPop = statesToPop.Dequeue()
                statesToPush.Enqueue stateToPop
                [Pop, stateToPop]
            else
                let rec tryGetPush() =
                    if statesToPush.Count = 0 then []
                    else
                        let next = statesToPush.Dequeue()
                        let pushes =
                            seq {
                                match tryGetThisConstructorToPush next with
                                | None ->
                                    match tryGetConstructorPush next with
                                    | None -> ()
                                    | Some action -> yield action, next
                                | Some action -> yield action, next
                                match tryGetPropertySetterPush next with
                                | None -> ()
                                | Some action -> yield action, next
                            }
                            |> Seq.toList
                        match pushes with
                        | [] -> tryGetPush()
                        | _ ->
                            statesToPush.Enqueue next
                            pushes
                tryGetPush()
        else
            let stateToForward = statesInMethod[0]
            [Forward, stateToForward]

    let checkTarget (target : cilState) (state : methodSequenceState) =
        assert(state.cilState.currentLoc = { method = targetMethod; offset = 0<offsets> })
        let wlp = Memory.WLP state.cilState.state target.state.pc
        let prevPc = state.cilState.state.pc
        state.cilState.state.pc <- wlp
        try
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
                    | Variable(TemporaryLocalVariableKey(typ, index) as key) when MethodSequenceHelpers.isPrimitive typ ->
                        match Memory.ReadLocalVariable primitiveModelState key with
                        | {term = Concrete(v, t)} as term ->
                            ConcretePrimitive(t, v)
                        | _ -> __unreachable__()
                    | _ -> arg

                let fillHoles (element : methodSequenceElement) =
                    match element with
                    | Call(method, stackKeyOption, methodSequenceArguments) ->
                        let mappedArguments = methodSequenceArguments |> List.map mapArgument
                        Call(method, stackKeyOption, mappedArguments)

                // TODO: we need to do something like compose with modelState and state

                let filledSequence = state.currentSequence |> List.map fillHoles
                target.state.model <- StateModel(modelState, currentTypeModel, Some filledSequence)
                finishedTargets[target] <- state
                true
            | _ ->
                false
        finally
            state.cilState.state.pc <- prevPc

    let update (parent : methodSequenceState) (newStates : methodSequenceState list) =
        match newStates with
        // Finished (entered target method)
        | _ when not parent.isInMethod && List.forall (fun newState -> newState.isInMethod && newState.cilState.currentLoc.method = targetMethod) newStates ->
            newStates |> List.iter (fun s -> finishedStates.Add(s))
            // TODO: more effectively?
            let finishedTargets = targets |> Seq.filter (fun tgt -> newStates |> List.exists (checkTarget tgt)) |> Seq.toList
            finishedTargets |> List.iter (targets.Remove >> ignore)
        // Entered new method
        | _ when not parent.isInMethod && List.forall (fun newState -> newState.isInMethod) newStates ->
            statesInMethod.AddRange newStates
        // Just exited from method
        | [newState] when parent.isInMethod && not newState.isInMethod->
            statesInMethod.Remove parent |> ignore
            match parent.methodsToCall with
            | [] -> statesToFinish.Enqueue newState
            | _ -> statesToPop.Enqueue newState
        // Inside method
        | _ when parent.isInMethod ->
            assert(List.forall (fun newState -> newState.isInMethod) newStates)
            statesInMethod.AddRange newStates
        // Push
        | [newState] when not parent.isInMethod ->
            assert(not <| List.isEmpty newState.methodsToCall)
            statesToPop.Enqueue newState
        | _ -> __unreachable__()

    let addTarget (parent : cilState option) (target : cilState) =
        if targets.Contains target then false
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
                    false
                else true
            else true

    interface IMethodSequenceSearcher with
        member x.Pick() = pick()
        member x.Update parent newStates = update parent newStates
        member x.AddTarget parent target = addTarget parent target

type internal MethodSequenceGenerator(searcherFactory : Method -> IMethodSequenceSearcher, interpreter : ILInterpreter) =

    let searchers = Dictionary<Method, IMethodSequenceSearcher>()

    let isAvailableInPublicApi (method : Method) =
        method.IsPublic && (method.DeclaringType.IsPublic || method.DeclaringType.IsNestedPublic)

    let canHaveDefaultThis (method : Method) =
        not method.HasThis || method.DeclaringType.IsValueType

    // Consider Nullable<>
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
                        if MethodSequenceHelpers.isPrimitive currentParameter.ParameterType ||
                           currentParameter.ParameterType.IsByRef && MethodSequenceHelpers.isPrimitive <| currentParameter.ParameterType.GetElementType() then
                            let argumentTerm =
                                if currentParameter.ParameterType.IsByRef then
                                    let key = ParameterKey currentParameter
                                    let argumentTerm = Memory.ReadLocalVariable cilState.state key
                                    Memory.Read modelState argumentTerm
                                else
                                    Memory.ReadArgument modelState currentParameter |> cilState.state.model.Complete
                            match argumentTerm with
                                | {term = Concrete(v, _)} ->
                                    readArgument remaining (ConcretePrimitive(currentParameter.ParameterType, v) :: acc)
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
                                readArgument remaining (Default(currentParameter.ParameterType) :: acc)
                readArgument (targetMethod.Parameters |> Seq.toList) []
            if targetMethod.HasThis then
                match readThis() with
                | None -> false
                | Some thisArg ->
                    match readArguments() with
                    | None -> false
                    | Some arguments ->
                        assert(arguments.Length = targetMethod.Parameters.Length)
                        let call = Call(targetMethod, None, thisArg :: arguments)
                        cilState.state.model <- StateModel(modelState, typeModel, Some [call])
                        true
            else
                match readArguments() with
                | None -> false
                | Some arguments ->
                    assert(arguments.Length = targetMethod.Parameters.Length)
                    let call = Call(targetMethod, None, arguments)
                    cilState.state.model <- StateModel(modelState, typeModel, Some [call])
                    true

    let isExitingFromMethod (state : methodSequenceState) =
        let baseMethod = Application.getMethod Loader.MethodSequenceBase
        match state.cilState.ipStack with
        // Может быть exit из m?
        | Exit _ :: Instruction(_, m) :: _ when m = baseMethod -> true
        | _ -> false

    let makeStepInsideMethod (state : methodSequenceState) =
        // Чекнуть, что с тем стейтом, что приходит
        let goodStates, iieStates, errors =
            try
                interpreter.ExecuteOneInstruction state.cilState
            with
            | e ->
                Console.WriteLine $"Method sequence searcher internal fail: {e}"
                [], [], []
        match goodStates, iieStates, errors with
        | parent :: goodStates, _, _ ->
            seq {
                // Maybe we can avoid copying
                for goodState in goodStates -> { state with cilState = goodState }
            } |> Seq.toList
        | _ -> []

    let exitFromMethod (state : methodSequenceState) =
        let cilState = state.cilState
        let method = cilState.currentLoc.method

        // evaluation stack суммируется для всех фреймов
        match EvaluationStack.Length cilState.state.evaluationStack with
        | 0 -> ()
        | 1 ->
            let result = EvaluationStack.Pop cilState.state.evaluationStack |> fst
            let result = Types.Cast result method.ReturnType
            match state.currentSequence with
            | Call(lastMethod, Some(resultKey), _) :: _ when lastMethod = method ->
                Memory.WriteLocalVariable cilState.state resultKey result
            | _ -> __unreachable__()
        | _ -> __unreachable__()

        match cilState.ipStack with
        | Exit m :: ips' ->
            CilStateOperations.popFrameOf cilState
            CilStateOperations.setIpStack ips' cilState
        | _ -> __unreachable__()

        {
          state with
            isInMethod = false
        }

    let getPossibleArguments (state : methodSequenceState) (method : Method) =
        let usePrimitiveHole (typ : Type) (state : methodSequenceState) =
            let index = if state.locals.ContainsKey typ then state.locals[typ] else 0
            let name = $"arg_{typ.Name}_{index}"
            let ref = Memory.AllocateTemporaryLocalVariableOfType state.cilState.state name index typ
            let term = Memory.Read state.cilState.state ref
            (term, Variable <| TemporaryLocalVariableKey(typ, index)), {state with locals = state.locals.Add(typ, index + 1)}

        let useExistingVariable (typ : Type) (index : int) (state : methodSequenceState) =
            let key = TemporaryLocalVariableKey(typ, index)
            let term = Memory.ReadLocalVariable state.cilState.state key
            (term, Variable <| key), state

        let createOutVariable (typ : Type) (state : methodSequenceState) =
            // TODO: check value type case
            let index = if state.locals.ContainsKey typ then state.locals[typ] else 0
            let defaultTerm = Memory.AllocateDefaultClass state.cilState.state typ
            let ref = Memory.AllocateTemporaryLocalVariable state.cilState.state index typ defaultTerm
            let term = Memory.Read state.cilState.state ref
            (term, OutVariable <| TemporaryLocalVariableKey(typ, index)), {state with locals = state.locals.Add(typ, index + 1)}

        let createDefault (typ : Type) (state : methodSequenceState) =
            // TODO: value type case
            let term = Memory.AllocateConcreteObject state.cilState.state null typ
            (term, Default typ), state

        let getPossibleArgumentsForType (typ : Type) isThis =
            seq {
                if MethodSequenceHelpers.isPrimitive typ then
                    yield usePrimitiveHole typ
                else
                    // TODO: if it is value type, we can use default this
                    if not isThis then
                        yield createDefault typ
                    // TODO: check IsAssignable
                    if isThis && method.IsConstructor then
                        yield createOutVariable typ
                    elif state.locals.ContainsKey typ then
                        for i in 0..(state.locals[typ] - 1) -> useExistingVariable typ i
            } |> Seq.toList

        let rec getPossibleArgumentsForTypes (types : Type list) isThis =
            match types with
            | [] -> []
            | [currentType] ->
                getPossibleArgumentsForType currentType isThis |> List.map List.singleton
            | currentType :: remainingTypes ->
                List.allPairs (getPossibleArgumentsForType currentType isThis) (getPossibleArgumentsForTypes remainingTypes false) |>
                List.map (fun (current, ss) -> current :: ss)
        let thisAndParameters = seq {
            if method.HasThis then yield method.DeclaringType
            for pi in method.Parameters -> pi.ParameterType
        }
        getPossibleArgumentsForTypes (thisAndParameters |> Seq.toList) method.HasThis

    let call (state : cilState) (method : Method) (this : term option) (args : term list) =
        let callAfterStaticsInitialized (state : cilState) =
            interpreter.InitFunctionFrameCIL state method this (Some args)
            [state]
        interpreter.InitializeStatics state method.DeclaringType callAfterStaticsInitialized

    let allocateResultVar (method : Method) (state : methodSequenceState) =
        let returnType = method.ReturnType
        if returnType <> typeof<Void> && not <| MethodSequenceHelpers.isPrimitive returnType then
            let index = if state.locals.ContainsKey returnType then state.locals[returnType] else 0
            let name = $"ret_{returnType.Name}_{index}"
            Memory.AllocateTemporaryLocalVariableOfType state.cilState.state name index returnType |> ignore
            Some <| TemporaryLocalVariableKey(returnType, index), {state with locals = state.locals.Add(returnType, index + 1)}
        else
            None, state

    let pop (state : methodSequenceState) =
        match state.methodsToCall with
        | [] -> __unreachable__()
        | methodToCall :: remainingMethods ->
            seq {
                for argumentsInitializers in getPossibleArguments state methodToCall do
                    let newCoreState = Memory.CopyState state.cilState.state
                    let newCilState = { state.cilState with state = newCoreState; id = CilStateOperations.getNextStateId() }
                    let newState = {state with cilState = newCilState}

                    let folder curState action = action curState
                    let arguments, newState = List.mapFold folder newState argumentsInitializers
                    let argumentTerms, callArguments = arguments |> List.unzip
                    let resultKey, newState = allocateResultVar methodToCall newState

                    let this, argumentTerms =
                        if methodToCall.HasThis then
                            List.head argumentTerms |> Some, List.tail argumentTerms
                        else
                            None, argumentTerms

                    let [newCilState] = call newCilState methodToCall this argumentTerms

                    let call = Call(methodToCall, resultKey, callArguments)
                    yield
                        {
                          newState with
                            cilState = newCilState
                            isInMethod = true
                            currentSequence = call :: state.currentSequence
                            actions = HashSet()
                            methodsToCall = remainingMethods
                        }
            } |> Seq.toList

    let executeAction (action : methodSequenceExplorerAction) (state : methodSequenceState) =
        match action with
        | Forward ->
            assert state.isInMethod
            if isExitingFromMethod state then
                [exitFromMethod state]
            else
                makeStepInsideMethod state
        | Pop ->
            assert(not <| state.actions.Contains action)
            state.actions.Add action |> ignore
            pop state
        | Push method ->
            assert(not <| state.actions.Contains action)
            state.actions.Add action |> ignore
            [{ state with actions = HashSet(); methodsToCall = method :: state.methodsToCall }]

    // Parent is already changed!
    member x.GetSequenceOrEnqueue (parent : cilState option) (cilState : cilState) =
        match cilState.state.model with
        | StateModel(_, _, Some _) -> Exists
        | _ ->
            let targetMethod = CilStateOperations.entryMethodOf cilState
            if not <| isAvailableInPublicApi targetMethod then NotExist
            elif tryConvertSolverModelToSequence cilState then Exists
            else
                if not <| searchers.ContainsKey targetMethod then
                    searchers[targetMethod] <- searcherFactory targetMethod
                if searchers[targetMethod].AddTarget parent cilState then
                    Exists
                else Unknown

    member x.MakeStep() =
        // TODO: switch between methods
        if searchers.Count > 0 then
            let searcher = Seq.head searchers.Values
            let actions = searcher.Pick()
            match actions with
            | [] -> false
            | _ ->
                for action, state in actions do
                    let newStates = executeAction action state
                    searcher.Update state newStates
                true
        else
            false
