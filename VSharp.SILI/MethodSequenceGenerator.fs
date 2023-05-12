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

type internal methodSequenceState =
    {
        cilState : cilState
        upcomingSequence : methodSequenceElement list
        currentSequence : methodSequenceElement list
        id : uint
    }

    override x.ToString() =
        let mutable sb = StringBuilder($"[{x.id}\n")
        for called in x.currentSequence do
            sb <- sb.AppendLine $"\t{called}"
        sb <- sb.AppendLine "\t ---"
        for toCall in x.upcomingSequence do
            sb <- sb.AppendLine $"\t{toCall}"
        sb <- sb.Append "]"
        sb.ToString()

type internal methodSequenceAction =
    | GoForward
    | GoBackward

type internal methodSequenceResult =
    | NotExist
    | Exists
    | Unknown

type internal IMethodSequenceSearcher =
    abstract Pick : unit -> methodSequenceState option
    abstract Update : methodSequenceState -> methodSequenceState list -> cilState list
    abstract AddTarget : cilState option -> cilState -> methodSequenceResult
    abstract RemoveTarget : cilState -> bool

type internal IMethodSequenceForwardExplorer =
    abstract MakeStep : methodSequenceState -> methodSequenceState list

type internal IMethodSequenceBackwardExplorer =
    abstract MakeNextStep : unit -> methodSequenceState option

module internal MethodSequenceHelpers =

    let baseMethod = Application.getMethod Loader.MethodSequenceBase

    let variableIndices = Dictionary<Type, int>()

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

    let getElementMethod (element : methodSequenceElement) =
        match element with
        | methodSequenceElement.Call(method, _, _) -> Some method
        | _ -> None

    let getAllMethods (state : methodSequenceState) =
        seq {
            yield! state.currentSequence |> List.choose getElementMethod
            yield! state.upcomingSequence |> List.choose getElementMethod
        }

    let getThisAndParameterTypes (method : IMethod) =
        seq {
            if not method.IsConstructor && method.HasThis then
                yield method.DeclaringType

            yield! method.Parameters |> Array.map (fun pi -> pi.ParameterType)
        }

    let getReturnVar (element : methodSequenceElement) =
        match element with
        | Call(_, var, _) -> var
        | CreateDefaultStruct var -> Some var

    let getFreshVariableId (typ : Type) =
        let index = if variableIndices.ContainsKey typ then variableIndices[typ] else 0
        variableIndices[typ] <- index + 1
        { typ = typ; index = index }

    let createUnknownArgumentOfType (typ : Type) =
        if not <| isPrimitive typ then
            Hole typ
        else
            getFreshVariableId typ |> Variable

    let isInMethod (state : methodSequenceState) =
        state.cilState.currentLoc.method <> baseMethod

    let getExistingObjectIds (state : methodSequenceState) =
        // TODO: we should also consider out vars
        state.currentSequence |> List.choose getReturnVar

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

    let enqueueInitialState targetMethod =
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
                    | Variable({ typ = typ }) when MethodSequenceHelpers.isPrimitive typ ->
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
                        | Variable({ typ = typ; index = index }) when MethodSequenceHelpers.isPrimitive typ ->
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

    let removeTarget target =
        targets.Remove target

    interface IMethodSequenceSearcher with
        member x.Pick() = pick()
        member x.Update parent newStates = update parent newStates
        member x.AddTarget parent target = addTarget parent target
        member x.RemoveTarget target = removeTarget target

type internal MethodSequenceBackwardExplorer(state : methodSequenceState) =

    static let specialConstructors =
        dict [ (typeof<decimal>, typeof<decimal>.GetConstructor([|typeof<int>; typeof<int>; typeof<int>; typeof<bool>; typeof<byte>|])) ]

    let publicFlags =
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        BindingFlags.Public ||| BindingFlags.Instance

    // TODO: interface properties
    let getSettersCalls (id : variableId) =
        id.typ.GetProperties publicFlags |>
            Seq.map (fun pi -> (pi.GetSetMethod(), pi.PropertyType)) |>
            Seq.filter (fun (m, _) -> m <> null) |>
            Seq.map (fun (m, t) -> Call(Application.getMethod m :> IMethod, None, [Variable id; MethodSequenceHelpers.createUnknownArgumentOfType t]))

    let getExistingArgumentsForHole typ isThis = seq {
        if not isThis && not <| MethodSequenceHelpers.isStruct typ then
            yield Default typ
        yield! MethodSequenceHelpers.getExistingObjectIds state |> List.filter (fun t -> t.typ = typ) |> List.map Variable
    }

    let getConstructors (typ : Type) =
        if specialConstructors.ContainsKey typ then [specialConstructors[typ]]
        else
            typ.GetConstructors publicFlags |> Array.toList

    // TODO: what about interfaces?
    let getConstructorsCalls typ = seq {
        let resultVarId = MethodSequenceHelpers.getFreshVariableId typ
        if MethodSequenceHelpers.isStruct typ && not <| specialConstructors.ContainsKey typ then
            yield resultVarId, CreateDefaultStruct resultVarId
        for ctor in getConstructors typ |> Seq.sortBy (fun ctor -> ctor.GetParameters().Length) ->
            let arguments =
                ctor.GetParameters() |>
                    Array.map (fun p -> MethodSequenceHelpers.createUnknownArgumentOfType p.ParameterType) |>
                    Array.toList
            resultVarId, Call(Application.getMethod ctor :> IMethod, Some resultVarId, arguments)
    }

    let steps = seq {
        match state.upcomingSequence with
        | Call(method, res, arguments) :: _ ->
            let unwrapHoleType (i, argument) =
                match argument with
                | Hole typ when not <| MethodSequenceHelpers.isPrimitive typ -> Some(i, typ)
                | _ -> None
            match List.indexed arguments |> List.tryPick unwrapHoleType with
            | None ->
                let isTheSameSetter existing newSetter =
                    match newSetter, existing with
                    | Call(newMethod, _, [newReceiver; _]), Call(exisingMethod, _, [exisingReceiver; _]) ->
                        newMethod = exisingMethod && newReceiver = exisingReceiver
                    | _ -> false
                // If there is nothing to create, try to call setters
                let settersCalls =
                    MethodSequenceHelpers.getExistingObjectIds state |>
                        Seq.collect getSettersCalls |>
                        Seq.filter (fun e -> not <| List.exists (isTheSameSetter e) state.currentSequence) |>
                        Seq.filter (fun e -> not <| List.exists (isTheSameSetter e) state.upcomingSequence)
                for call in settersCalls ->
                    { state with upcomingSequence = call :: state.upcomingSequence }
            | Some(index, typ) ->
                let withArgument arg = arguments |> List.mapi (fun i current -> if i = index then arg else current)
                let isThis = method.HasThis && not method.IsConstructor
                let unwrappedType = MethodSequenceHelpers.unwrapRefType typ
                // First, try to substitute existing arguments (default or locals)
                for existingArgument in getExistingArgumentsForHole unwrappedType isThis ->
                    { state with upcomingSequence = Call(method, res, withArgument existingArgument) :: state.upcomingSequence.Tail }
                // Then, try to call different constructors
                for resultVar, ctorCall in getConstructorsCalls unwrappedType ->
                    { state with upcomingSequence = ctorCall :: Call(method, res, withArgument <| Variable resultVar) :: state.upcomingSequence.Tail }
        | CreateDefaultStruct _ :: _-> ()
        | _ -> __unreachable__()
    }

    let enumerator = steps.GetEnumerator()

    interface IMethodSequenceBackwardExplorer with
        override x.MakeNextStep() =
            if enumerator.MoveNext() then
                Some enumerator.Current
            else None

type internal MethodSequenceGenerator(interpreter : ILInterpreter) =

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
                let thisAndParameters = MethodSequenceHelpers.getThisAndParameterTypes targetMethod |> Seq.toArray
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

    // TODO: out vars
    (*let createOutVariable (typ : Type) (state : methodSequenceState) =
        // TODO: check value type case
        let index = if state.locals.ContainsKey typ then state.locals[typ] else 0
        let term =
            if MethodSequenceHelpers.isStruct typ then
                Memory.AllocateTemporaryLocalVariable state.cilState.state index typ (Memory.DefaultOf typ)
            else
                let defaultTerm = Memory.AllocateDefaultClass state.cilState.state typ
                let ref = Memory.AllocateTemporaryLocalVariable state.cilState.state index typ defaultTerm
                Memory.Read state.cilState.state ref
        (term, Variable <| { typ = typ; index = index }), {state with locals = state.locals.Add(typ, index + 1)}*)

    let pop (state : methodSequenceState) =
        let newCoreState = Memory.CopyState state.cilState.state
        let newCilState = { state.cilState with state = newCoreState; id = CilStateOperations.getNextStateId() }
        let newState = {state with cilState = newCilState}
        match state.upcomingSequence with
        | [] -> __unreachable__()
        | CreateDefaultStruct({ typ = typ; index = index }) as currentElement :: remainingMethods ->
            // TODO: if the next action is default struct creation then it makes sense to just create it an forget
            // about this state
            assert(MethodSequenceHelpers.isStruct typ)
            let name = $"ret_{typ.Name}_{index}"
            Memory.AllocateTemporaryLocalVariableOfType newCoreState name index typ |> ignore
            let resultKey = TemporaryLocalVariableKey(typ, index)
            Memory.WriteLocalVariable newCoreState resultKey (Memory.DefaultOf typ)
            [
                {
                    newState with
                        cilState = newCilState
                        currentSequence = currentElement :: state.currentSequence
                        upcomingSequence = remainingMethods
                        id = MethodSequenceHelpers.getNextStateId()
                }
            ]
        | Call(methodToCall, resVariable, arguments) as currentElement :: remainingMethods ->
            let isTargetMethod = remainingMethods.IsEmpty
            let hasThis = methodToCall.HasThis && not methodToCall.IsConstructor
            let argumentToTerm (index : int) (argument : methodSequenceArgument) =
                let isThis = hasThis && index = 0
                let parameterPosition = if hasThis then index - 1 else index
                let isByRef = not isThis && methodToCall.Parameters[parameterPosition].ParameterType.IsByRef

                let mapIndexForTargetMethod (term : term) (typ : Type) =
                    if isThis then
                        if Types.IsValueType typ then
                            let term = Memory.Read newCoreState term
                            Memory.AllocateTemporaryLocalVariable newCoreState -1 methodToCall.DeclaringType term
                        else term
                    else
                        let term = if Types.IsValueType typ then Memory.Read newCoreState term else term
                        Memory.AllocateTemporaryLocalVariable newCoreState (-parameterPosition - 2) typ term

                match argument with
                | Variable({ typ = typ; index = varIndex }) ->
                    let isByRef = isByRef || (MethodSequenceHelpers.isStruct typ && isThis)

                    if MethodSequenceHelpers.isPrimitive typ then
                        let name = $"arg_{typ.Name}_{varIndex}"
                        let ref = Memory.AllocateTemporaryLocalVariableOfType newCoreState name varIndex typ
                        Memory.Read newCoreState ref
                    else
                        let key = TemporaryLocalVariableKey(typ, varIndex)
                        let term =
                            if isByRef && MethodSequenceHelpers.isStruct typ then
                                PrimitiveStackLocation key |> Ref
                            else
                                Memory.ReadLocalVariable newCoreState key
                        if isTargetMethod && isByRef then
                            mapIndexForTargetMethod term typ
                        else
                            term
                | Default typ ->
                    assert not typ.IsValueType
                    let term = NullRef typ
                    if isTargetMethod && isByRef then
                        mapIndexForTargetMethod term typ
                    else
                        term
                | Hole _
                | ConcretePrimitive _ -> __unreachable__()

            match resVariable with
            | Some({ typ = typ; index = index }) ->
                let name = $"ret_{typ.Name}_{index}"
                Memory.AllocateTemporaryLocalVariableOfType newCoreState name index typ |> ignore
            | None -> ()

            let argumentTerms = arguments |> List.mapi argumentToTerm
            let wrapper = getWrapperMethod methodToCall
            interpreter.InitFunctionFrameCIL newCilState wrapper None (Some argumentTerms)
            [
                {
                    newState with
                        cilState = newCilState
                        currentSequence = currentElement :: state.currentSequence
                        upcomingSequence = remainingMethods
                        id = MethodSequenceHelpers.getNextStateId()
                }
            ]

    let makeStep (state : methodSequenceState) =
        // TODO: Try enable concrete memory with unmarshalling
        let wasConcreteMemoryEnabled = Memory.IsConcreteMemoryEnabled()
        let wereBranchesReleased = BranchesReleased()
        AcquireBranches()
        Memory.EnableConcreteMemory false
        try
            if MethodSequenceHelpers.isInMethod state then
                if isExitingFromMethod state then
                    exitFromMethod state
                    [state]
                else
                    makeStepInsideMethod state
            else
                pop state
        finally
            Memory.EnableConcreteMemory wasConcreteMemoryEnabled
            if wereBranchesReleased then ReleaseBranches()

    interface IMethodSequenceForwardExplorer with
        override x.MakeStep state = makeStep state
