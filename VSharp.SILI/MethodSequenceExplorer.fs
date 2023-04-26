namespace VSharp.MethodSequences

open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit
open System.Text
open FSharpx.Collections
open Microsoft.FSharp.Collections
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL

type internal methodSequenceAction =
    | Call of IMethod
    | CreateDefaultStruct of Type

type internal explorerAction =
    | Forward
    | Pop
    | Push of methodSequenceAction

type internal methodSequenceState =
    {
        cilState : cilState
        isInMethod : bool
        methodsToCall : methodSequenceAction list
        currentSequence : methodSequenceElement list
        locals : PersistentHashMap<Type, int>
        actions : HashSet<explorerAction>
        id : uint
    }

    override x.ToString() =
        let mutable sb = StringBuilder($"[{x.id} actions: {Seq.toList x.actions}\n")
        for called in x.currentSequence do
            sb <- sb.AppendLine $"\t{called}"
        sb <- sb.AppendLine "\t ---"
        for toCall in x.methodsToCall do
            match toCall with
            | Call method ->
                sb <- sb.AppendLine $"\t{method.DeclaringType.Name}.{method.Name}"
            | CreateDefaultStruct typ ->
                sb <- sb.AppendLine $"\tdefault({typ.Name})"
        sb <- sb.Append "]"
        sb.ToString()

type internal methodSequenceResult =
    | NotExist
    | Exists
    | Unknown

type internal IMethodSequenceSearcher =
    abstract Pick : unit -> (explorerAction * methodSequenceState) list
    abstract Update : methodSequenceState -> methodSequenceState list -> cilState list
    abstract AddTarget : cilState option -> cilState -> methodSequenceResult

type internal IMethodSequenceExplorer =
    abstract ExecuteAction : explorerAction -> methodSequenceState -> methodSequenceState list

module internal MethodSequenceHelpers =

    let mutable private currentId = 0u

    // TODO: move to reflection
    let isPrimitive (t : Type) = t.IsPrimitive || t.IsEnum

    let isStruct (t : Type) = t.IsValueType && not t.IsPrimitive && not t.IsEnum

    let unwrapRefType (typ : Type) =
        if typ.IsByRef then typ.GetElementType()
        else typ

    let getNextStateId() =
        currentId <- currentId + 1u
        currentId

    let getAllMethods (state : methodSequenceState) =
        let getElementMethod (element : methodSequenceElement) =
            match element with
            | methodSequenceElement.Call(method, _, _) -> Some method
            | _ -> None

        let getActionMethod (action : methodSequenceAction) =
            match action with
            | Call method -> Some method
            | _ -> None

        seq {
            yield! state.currentSequence |> List.choose getElementMethod
            yield! state.methodsToCall |> List.choose getActionMethod
        }

// TODO: если нет таргетов, то и не pick-ать?
type MethodSequenceSearcher(maxSequenceLength : uint) =

    let targetMethods = HashSet<IMethod>()

    let targets = List<cilState>()

    let finishedTargets = Dictionary<cilState, methodSequenceState>()

    // TODO: sort!
    let finishedStates = List<methodSequenceState>()

    // States to go forward in terms of symbolic execution
    // isInMethod == true
    let statesInMethod = Queue<methodSequenceState>()

    // States to pop the target method
    // isInMethod == false; > 0 elements in sequence; exactly 1 (target) method to call
    let statesToFinish = Queue<methodSequenceState>()

    // States to pop the next method and start its symbolic execution
    // isInMethod == false; >= 0 elements in sequence; > 1 methods to call
    let statesToPop = Queue<methodSequenceState>()

    // States to push a new method (i. e. constructor or setter)
    // isInMethod == false; >= 0 elements in sequence; >= 1 methods to call
    let statesToPush = PriorityQueue<methodSequenceState, int>()

    let mutable counter = 0
    let interleaveAt = 10

    let enqueueInitialState targetMethod =
        let initialCoreState = Memory.EmptyModelState()
        let baseMethod = Application.getMethod Loader.MethodSequenceBase
        initialCoreState.model <- Memory.EmptyModel baseMethod (typeModel.CreateEmpty())
        let initialCilState = CilStateOperations.makeInitialState baseMethod initialCoreState
        ILInterpreter.InitFunctionFrame initialCoreState baseMethod None None
        let targetMethodCall = Call targetMethod
        let initialState =
            {
                cilState = initialCilState
                isInMethod = false
                methodsToCall = [targetMethodCall]
                currentSequence = List.empty
                locals = PersistentHashMap.empty
                actions = HashSet()
                id = MethodSequenceHelpers.getNextStateId()
            }
        statesToPush.Enqueue(initialState, 1)

    let canHaveDefaultThis (method : Method) =
        not method.HasThis || method.DeclaringType.IsValueType

    let isAvailableInPublicApi (method : Method) =
        method.IsPublic && (method.DeclaringType.IsPublic || method.DeclaringType.IsNestedPublic)

    let canPushOn (state : methodSequenceState) =
        uint (state.currentSequence.Length + state.methodsToCall.Length) <= maxSequenceLength

    let publicFlags =
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        BindingFlags.Public ||| BindingFlags.Instance

    let getNextMethod (state : methodSequenceState) =
        match state.methodsToCall with
        | Call method :: _ -> Some method
        | CreateDefaultStruct _ :: _ -> None
        | _ -> __unreachable__()

    let getConstructors (typ : Type) =
        if typ = typeof<decimal> then
            let ctor = typeof<decimal>.GetConstructor([|typeof<int>; typeof<int>; typeof<int>; typeof<bool>; typeof<byte>|])
            [ctor]
        else
            typ.GetConstructors publicFlags |> Array.toList

    let addDefaultCtor (typ : Type) ctors =
        if MethodSequenceHelpers.isStruct typ && typ <> typeof<decimal> then
            Seq.cons (methodSequenceAction.CreateDefaultStruct typ |> Push) ctors
        else ctors

    let tryGetThisConstructorToPush (state : methodSequenceState) =
        match getNextMethod state with
        | Some nextMethod when nextMethod.HasThis && not nextMethod.IsConstructor && not <| state.locals.ContainsKey nextMethod.DeclaringType ->
            let typ = nextMethod.DeclaringType
            getConstructors typ |>
                Seq.sortBy (fun ctor -> ctor.GetParameters().Length) |>
                Seq.map Application.getMethod |>
                Seq.cast<IMethod> |>
                Seq.map (Call >> Push) |>
                addDefaultCtor typ |>
                Seq.tryFind (state.actions.Contains >> not)
        | _ -> None

    let tryGetConstructorPush (state : methodSequenceState) =
        // TODO: use some smarter logic here
        // In fact we need to look at all methods to call
        // TODO: what about constructors with default parameters?
        match getNextMethod state with
        | None -> None
        | Some nextMethod ->
            let getConstructorPushes (typ : Type) =
                getConstructors typ |>
                    Seq.sortBy (fun ctor -> ctor.GetParameters().Length) |>
                    Seq.map Application.getMethod |>
                    Seq.cast<IMethod> |>
                    Seq.map (Call >> Push) |>
                    addDefaultCtor typ
            nextMethod.Parameters |>
                Seq.map (fun pi -> pi.ParameterType) |>
                Seq.filter (MethodSequenceHelpers.isPrimitive >> not) |>
                Seq.map MethodSequenceHelpers.unwrapRefType |>
                Seq.countBy id |>
                Seq.filter (fun (typ, count) -> not <| state.locals.ContainsKey typ || state.locals[typ] < count) |>
                Seq.collect (fun (typ, _) -> getConstructorPushes typ) |>
                Seq.tryFind (state.actions.Contains >> not)

    let tryGetPropertySetterPush (state : methodSequenceState) =
        let canPushSetter (state : methodSequenceState) (setter : IMethod) =
            let currentSettersCount = MethodSequenceHelpers.getAllMethods state |> Seq.filter (fun m -> m = setter) |> Seq.length
            let localsCount = if state.locals.ContainsKey setter.DeclaringType then state.locals[setter.DeclaringType] else 0
            currentSettersCount < localsCount

        state.locals.Iterator() |>
            Seq.map fst |>
            Seq.collect (fun typ -> typ.GetProperties publicFlags) |>
            Seq.map (fun pi -> pi.GetSetMethod()) |>
            Seq.filter (fun m -> m <> null) |>
            Seq.map Application.getMethod |>
            Seq.cast<IMethod> |>
            Seq.filter (canPushSetter state) |>
            Seq.map (Call >> Push) |>
            Seq.tryFind (not << state.actions.Contains)

    let pick() =
        if targets.Count = 0 then []
        else
            counter <- (counter + 1) % interleaveAt
            if statesInMethod.Count = 0 || counter = interleaveAt then
                Logger.traceWithTag Logger.methodSequenceSearcherTag $"To pop: {statesToPop.Count}; To push: {statesToPush.Count}; To finish {statesToFinish.Count}; Finished: {finishedStates.Count}"
                if statesToFinish.Count > 0 then
                    let stateToFinish = statesToFinish.Dequeue()
                    assert(stateToFinish.methodsToCall.Length = 1)
                    statesToPush.Enqueue(stateToFinish, stateToFinish.currentSequence.Length + stateToFinish.methodsToCall.Length)
                    Logger.traceWithTag Logger.methodSequenceSearcherTag $"PICK (TO FINISH): \n{stateToFinish}"
                    [Pop, stateToFinish]
                elif statesToPop.Count > 0 then
                    let stateToPop = statesToPop.Dequeue()
                    statesToPush.Enqueue(stateToPop, stateToPop.currentSequence.Length + stateToPop.methodsToCall.Length)
                    Logger.traceWithTag Logger.methodSequenceSearcherTag $"PICK (TO POP): \n{stateToPop}"
                    [Pop, stateToPop]
                else
                    let rec tryGetPush() =
                        if statesToPush.Count = 0 then []
                        else
                            // TODO: what if we can't push? We lose the state?
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
                                statesToPush.Enqueue(next, next.currentSequence.Length + next.methodsToCall.Length)
                                pushes
                    let push = tryGetPush()
                    Logger.traceWithTag Logger.methodSequenceSearcherTag $"PICK (TO PUSH): \n{snd push.Head} ({push.Length})"
                    push
            else
                let stateToForward = statesInMethod.Dequeue()
                [Forward, stateToForward]

    let checkTarget (target : cilState) (state : methodSequenceState) =
        assert(state.cilState.currentLoc.offset = 0<offsets> && targetMethods.Contains state.cilState.currentLoc.method)
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
                    | Variable({ typ = typ; index = index }) when MethodSequenceHelpers.isPrimitive typ ->
                        let key = TemporaryLocalVariableKey(typ, index)
                        match Memory.ReadLocalVariable primitiveModelState key with
                        | {term = Concrete(v, t)} -> ConcretePrimitive(t, v)
                        | _ -> __unreachable__()
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
            assert(parent.isInMethod && newState.isInMethod)
            finish newState
        | [{ isInMethod = false } as newState] ->
            match newState.methodsToCall with
            | [Call nextMethod] when targetMethods.Contains nextMethod ->
                statesToFinish.Enqueue newState
            | _ :: _ :: _ ->
                statesToPop.Enqueue newState
            | _ -> __unreachable__()
            []
        | _ :: _ when List.forall (fun newState -> newState.isInMethod) newStates ->
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

    interface IMethodSequenceSearcher with
        member x.Pick() = pick()
        member x.Update parent newStates = update parent newStates
        member x.AddTarget parent target = addTarget parent target

type internal MethodSequenceExplorer(interpreter : ILInterpreter) =

    let wrappersModuleBuilder =
        let wrappersAssemblyName = "MethodSequenceWrappers"
        let assemblyBuilder = AssemblyManager.DefineDynamicAssembly(AssemblyName wrappersAssemblyName, AssemblyBuilderAccess.Run)
        assemblyBuilder.DefineDynamicModule wrappersAssemblyName

    let isExitingFromMethod (state : methodSequenceState) =
        let baseMethod = Application.getMethod Loader.MethodSequenceBase
        match state.cilState.ipStack with
        // Может быть exit из m?
        | Exit _ :: Instruction(_, m) :: _ when m = baseMethod -> true
        | _ -> false

    let makeStepInsideMethod (state : methodSequenceState) =
        try
            seq {
                let goodStates, _, _ = interpreter.ExecuteOneInstruction state.cilState
                let goodStates, _ = goodStates |> List.partition CilStateOperations.isExecutable
                match goodStates with
                | s'::goodStates when LanguagePrimitives.PhysicalEquality state.cilState s' ->
                    yield state
                    for goodState in goodStates -> { state with cilState = goodState }
                | _ ->
                    for goodState in goodStates -> { state with cilState = goodState }
            } |> Seq.toList
        with
        | :? UnknownMethodException as e ->
            Console.WriteLine $"Method sequence generator exception: {e.Message}"
            []

    let exitFromMethod (state : methodSequenceState) =
        let cilState = state.cilState
        let method = cilState.currentLoc.method

        // TODO: evaluation stack суммируется для всех фреймов
        match EvaluationStack.Length cilState.state.evaluationStack with
        | 0 -> ()
        | 1 ->
            let result, newStack = EvaluationStack.Pop cilState.state.evaluationStack
            cilState.state.evaluationStack <- newStack
            let result = Types.Cast result (if method.IsConstructor then method.DeclaringType else method.ReturnType)
            match state.currentSequence with
            | methodSequenceElement.Call(_, Some({ typ = resultType; index = resultIdx }), _) :: _->
                let resultKey = TemporaryLocalVariableKey(resultType, resultIdx)
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
            actions = HashSet()
        }

    let getPossibleArguments (state : methodSequenceState) (method : IMethod) =
        let usePrimitiveHole (typ : Type) (state : methodSequenceState) =
            let index = if state.locals.ContainsKey typ then state.locals[typ] else 0
            let name = $"arg_{typ.Name}_{index}"
            let ref = Memory.AllocateTemporaryLocalVariableOfType state.cilState.state name index typ
            let term = Memory.Read state.cilState.state ref
            (term, Variable <| { typ = typ; index = index }), {state with locals = state.locals.Add(typ, index + 1)}

        let useExistingVariable (typ : Type) (index : int) (createRef : bool) (state : methodSequenceState) =
            let key = TemporaryLocalVariableKey(typ, index)
            let term =
                if MethodSequenceHelpers.isStruct typ && createRef then
                    PrimitiveStackLocation key |> Ref
                else
                    Memory.ReadLocalVariable state.cilState.state key
            (term, Variable <| { typ = typ; index = index }), state

        let createOutVariable (typ : Type) (state : methodSequenceState) =
            // TODO: check value type case
            let index = if state.locals.ContainsKey typ then state.locals[typ] else 0
            let term =
                if MethodSequenceHelpers.isStruct typ then
                    Memory.AllocateTemporaryLocalVariable state.cilState.state index typ (Memory.DefaultOf typ)
                else
                    let defaultTerm = Memory.AllocateDefaultClass state.cilState.state typ
                    let ref = Memory.AllocateTemporaryLocalVariable state.cilState.state index typ defaultTerm
                    Memory.Read state.cilState.state ref
            (term, Variable <| { typ = typ; index = index }), {state with locals = state.locals.Add(typ, index + 1)}

        let createDefault (typ : Type) (state : methodSequenceState) =
            let term =
                if typ.IsValueType then Memory.DefaultOf typ
                else NullRef typ
            (term, Default typ), state

        let getPossibleArgumentsForType (typ : Type) isThis =
            let unwrapped = MethodSequenceHelpers.unwrapRefType typ
            seq {
                if MethodSequenceHelpers.isPrimitive unwrapped then
                    yield usePrimitiveHole typ
                else
                    if not isThis && not <| MethodSequenceHelpers.isStruct unwrapped then
                        yield createDefault unwrapped
                    // TODO: check IsAssignable
                    // TODO: value type constructor
                    (*if isThis && method.IsConstructor then
                        yield createOutVariable unwrapped*)
                    if state.locals.ContainsKey unwrapped then
                        for i in 0..(state.locals[unwrapped] - 1) -> useExistingVariable unwrapped i (isThis || typ.IsByRef)
            } |> Seq.toList

        let rec getPossibleArgumentsForTypes (types : Type list) isThis =
            match types with
            | [] -> [[]]
            | [currentType] ->
                getPossibleArgumentsForType currentType isThis |> List.map List.singleton
            | currentType :: remainingTypes ->
                List.allPairs (getPossibleArgumentsForType currentType isThis) (getPossibleArgumentsForTypes remainingTypes false) |>
                List.map (fun (current, ss) -> current :: ss)
        let thisAndParameters = seq {
            if method.HasThis && not method.IsConstructor then yield method.DeclaringType
            for pi in method.Parameters -> pi.ParameterType
        }
        getPossibleArgumentsForTypes (thisAndParameters |> Seq.toList) (method.HasThis && not method.IsConstructor)

    let getWrapperMethod (targetMethod : IMethod) =
        let wrapperTypeName = $"Wrapper-{targetMethod.DeclaringType.MetadataToken}-{targetMethod.MetadataToken}"
        let wrapperMethodName = "Call"
        let existingType = wrappersModuleBuilder.GetType(wrapperTypeName, false, false)
        let typ =
            if existingType <> null then existingType
            else
                let typeBuilder = wrappersModuleBuilder.DefineType wrapperTypeName
                let (|||) = Microsoft.FSharp.Core.Operators.(|||)
                let methodBuilder = typeBuilder.DefineMethod(wrapperMethodName, MethodAttributes.Public ||| MethodAttributes.Static)
                // TODO: move to utils
                let thisAndParameters =
                    seq {
                        if targetMethod.HasThis && not targetMethod.IsConstructor then yield targetMethod.DeclaringType
                        yield! (targetMethod.Parameters |> Array.map (fun pi -> pi.ParameterType))
                    }
                    |> Seq.toArray
                methodBuilder.SetParameters thisAndParameters
                let returnType = if targetMethod.IsConstructor then targetMethod.DeclaringType else targetMethod.ReturnType
                methodBuilder.SetReturnType returnType
                let generator = methodBuilder.GetILGenerator()
                for i in 0..(thisAndParameters.Length - 1) do
                    generator.Emit(OpCodes.Ldarg, i)
                // TODO: won't work with generics
                // TODO: won't work with arrays
                if targetMethod.IsConstructor then
                    let ctorInfo = targetMethod.MethodBase :?> ConstructorInfo
                    generator.Emit(OpCodes.Newobj, ctorInfo)
                else
                    let opCode =
                        if targetMethod.IsVirtual then OpCodes.Callvirt
                        else OpCodes.Call
                    let methodInfo = targetMethod.MethodBase :?> MethodInfo
                    generator.Emit(opCode, methodInfo)

                generator.Emit OpCodes.Ret
                typeBuilder.CreateType()
        typ.GetMethod wrapperMethodName |> Application.getMethod

    let call20 (state : cilState) (method : IMethod) (this : term option) (args : term list) (isTarget : bool) =
        let wrapper = getWrapperMethod method

        let mapThis (thisTerm : term) =
            if isTarget && Types.IsValueType method.DeclaringType then
                let term = Memory.Read state.state thisTerm
                Memory.AllocateTemporaryLocalVariable state.state -1 method.DeclaringType term
            else thisTerm

        let mapArgument (idx : int) (argTerm : term) =
            if not isTarget then
                argTerm
            else
                let parameterInfo = method.Parameters[idx]
                // TODO: its partially copy-pasted from SILI, maybe we can share some code
                if parameterInfo.ParameterType.IsByRef then
                    let elementType = parameterInfo.ParameterType.GetElementType()
                    let term =
                        if Types.IsValueType elementType then
                            Memory.Read state.state argTerm
                        else
                            argTerm
                    Memory.AllocateTemporaryLocalVariable state.state (-parameterInfo.Position - 1) elementType term
                else
                    argTerm

        let args =
            seq {
                match this with
                | Some thisTerm -> yield mapThis thisTerm
                | None -> ()
                yield! args |> List.mapi mapArgument
            } |> Seq.toList
        interpreter.InitFunctionFrameCIL state wrapper None (Some args)

    let allocateResultVar (method : IMethod) (state : methodSequenceState) =
        let returnType = if method.IsConstructor then method.DeclaringType else method.ReturnType
        if returnType <> typeof<Void> && not <| MethodSequenceHelpers.isPrimitive returnType then
            let index = if state.locals.ContainsKey returnType then state.locals[returnType] else 0
            let name = $"ret_{returnType.Name}_{index}"
            Memory.AllocateTemporaryLocalVariableOfType state.cilState.state name index returnType |> ignore
            Some <| { typ = returnType; index = index }, {state with locals = state.locals.Add(returnType, index + 1)}
        else
            None, state

    let pop (state : methodSequenceState) =
        match state.methodsToCall with
        | [] -> __unreachable__()
        | CreateDefaultStruct typ :: remainingMethods ->
            // TODO: don't we need static ctor? Seems like no
            let newCoreState = Memory.CopyState state.cilState.state
            let newCilState = { state.cilState with state = newCoreState; id = CilStateOperations.getNextStateId() }
            let newState = {state with cilState = newCilState}
            // TODO: if the next action is default struct creation then it makes sense to just create it an forget
            // about this state
            assert(MethodSequenceHelpers.isStruct typ)
            let index = if state.locals.ContainsKey typ then state.locals[typ] else 0
            let name = $"ret_{typ.Name}_{index}"
            Memory.AllocateTemporaryLocalVariableOfType newCoreState name index typ |> ignore
            let resultKey = TemporaryLocalVariableKey(typ, index)
            Memory.WriteLocalVariable newCoreState resultKey (Memory.DefaultOf typ)
            let seqElement = methodSequenceElement.CreateDefaultStruct({ typ = typ; index = index })
            [
                {
                  newState with
                    cilState = newCilState
                    isInMethod = false
                    currentSequence = seqElement :: state.currentSequence
                    actions = HashSet()
                    methodsToCall = remainingMethods
                    id = MethodSequenceHelpers.getNextStateId()
                    locals = state.locals.Add(typ, index + 1)
                }
            ]

        | Call methodToCall :: remainingMethods ->
            seq {
                for argumentsInitializers in getPossibleArguments state methodToCall do
                    let newCoreState = Memory.CopyState state.cilState.state
                    let newCilState = { state.cilState with state = newCoreState; id = CilStateOperations.getNextStateId() }
                    let newState = {state with cilState = newCilState}
                    let folder curState action = action curState
                    let arguments, newState = List.mapFold folder newState argumentsInitializers
                    let argumentTerms, callArguments = arguments |> List.unzip
                    let resultVar, newState = allocateResultVar methodToCall newState

                    let this, argumentTerms =
                        // TODO: move this condition to function?
                        if methodToCall.HasThis && not methodToCall.IsConstructor then
                            List.head argumentTerms |> Some, List.tail argumentTerms
                        else
                            None, argumentTerms

                    let isTargetMethod = remainingMethods.IsEmpty
                    call20 newCilState methodToCall this argumentTerms isTargetMethod

                    let call = methodSequenceElement.Call(methodToCall, resultVar, callArguments)
                    yield
                        {
                          newState with
                            cilState = newCilState
                            isInMethod = true
                            currentSequence = call :: state.currentSequence
                            actions = HashSet()
                            methodsToCall = remainingMethods
                            id = MethodSequenceHelpers.getNextStateId()
                        }
            } |> Seq.toList

    let executeAction (action : explorerAction) (state : methodSequenceState) =
        // TODO: Try enable concrete memory with unmarshalling
        let wasConcreteMemoryEnabled = Memory.IsConcreteMemoryEnabled()
        Memory.EnableConcreteMemory false
        try
            match action with
            | Forward ->
                assert state.isInMethod
                if isExitingFromMethod state then
                    (*Logger.traceWithTag Logger.methodSequenceSearcherTag "[Generator] Exit from current method:"
                    Logger.traceWithTag Logger.methodSequenceSearcherTag $"{state}"*)
                    [exitFromMethod state]
                else
                    (*Logger.traceWithTag Logger.methodSequenceSearcherTag "[Generator] Make step in method:"
                    Logger.traceWithTag Logger.methodSequenceSearcherTag $"{state}"*)
                    makeStepInsideMethod state
            | Pop ->
                (*Logger.traceWithTag Logger.methodSequenceSearcherTag "[Generator] Pop:"
                Logger.traceWithTag Logger.methodSequenceSearcherTag $"{state}"*)
                pop state
            | Push method ->
                assert(not <| state.actions.Contains action)
                (*Logger.traceWithTag Logger.methodSequenceSearcherTag $"[Generator] Push {method}:"
                Logger.traceWithTag Logger.methodSequenceSearcherTag $"{state}"*)
                state.actions.Add action |> ignore
                [{ state with actions = HashSet(); methodsToCall = method :: state.methodsToCall; id = MethodSequenceHelpers.getNextStateId() }]
        finally
            Memory.EnableConcreteMemory wasConcreteMemoryEnabled

    interface IMethodSequenceExplorer with
        override x.ExecuteAction action state = executeAction action state
