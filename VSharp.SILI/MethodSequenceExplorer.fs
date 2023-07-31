namespace VSharp.MethodSequences

open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit
open FSharpx.Collections
open Microsoft.FSharp.Collections
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.MethodSequences

type internal IMethodSequenceForwardExplorer =
    abstract MakeStep : methodSequenceState -> methodSequenceState list

type internal IMethodSequenceBackwardExplorer =
    abstract MakeNextStep : unit -> methodSequenceState option

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
            Seq.map (fun (m, t) ->
                Call(Application.getMethod m :> IMethod, None, Some <| Variable id, [MethodSequenceHelpers.createUnknownArgumentOfType t]))

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
            resultVarId, Call(Application.getMethod ctor :> IMethod, Some resultVarId, None, arguments)
    }

    let steps = seq {
        match state.upcomingSequence with
        | Call(method, res, this, arguments) :: remainingMethods ->
            let isTargetMethod = List.isEmpty remainingMethods
            let unmatchedHole =
                let unwrapHoleType (i, argument) =
                    match argument with
                    | Hole typ -> Some(typ, false, i)
                    | _ -> None
                match this with
                | Some(Hole typ) -> Some(typ, true, -1)
                | _ -> List.indexed arguments |> List.tryPick unwrapHoleType
            match unmatchedHole with
            | None ->
                let isTheSameSetter existing newSetter =
                    match newSetter, existing with
                    | Call(newMethod, _, Some newReceiver, _), Call(exisingMethod, _, Some exisingReceiver, _) ->
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
            | Some(typ, isThis, index) ->
                let withArgument arg = arguments |> List.mapi (fun i current -> if i = index then arg else current)
                // First, try to substitute with existing arguments (default or locals)
                for existingArgument in getExistingArgumentsForHole typ (MethodSequenceHelpers.hasInstanceThis method) ->
                    let call =
                        if isThis
                        then Call(method, res, Some existingArgument, arguments)
                        else Call(method, res, this, withArgument existingArgument)
                    { state with upcomingSequence = call :: state.upcomingSequence.Tail }
                // Then, try to call different constructors
                for resultVar, ctorCall in getConstructorsCalls typ ->
                    let resultArg = Variable resultVar
                    let call =
                        if isThis
                        then Call(method, res, Some resultArg, arguments)
                        else Call(method, res, this, withArgument resultArg)
                    { state with upcomingSequence = ctorCall :: call :: state.upcomingSequence.Tail }
        | CreateDefaultStruct _ :: _-> ()
        | _ -> __unreachable__()
    }

    let enumerator = steps.GetEnumerator()

    interface IMethodSequenceBackwardExplorer with
        override x.MakeNextStep() =
            if enumerator.MoveNext() then
                Some enumerator.Current
            else None

type internal MethodSequenceForwardExplorer(interpreter : ILInterpreter) =

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
            | methodSequenceElement.Call(_, Some({ typ = resultType; index = resultIdx }), _, _) :: _->
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
            // TODO: if the next action is default struct creation then it makes sense to just create it and forget
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
        | Call(methodToCall, resVariable, this, arguments) as currentElement :: remainingMethods ->
            match resVariable with
            | Some({ typ = typ; index = index }) ->
                let name = $"ret_{typ.Name}_{index}"
                Memory.AllocateTemporaryLocalVariableOfType newCoreState name index typ |> ignore
            | None -> ()

            let aliases = List<variableId * int>()

            let isTargetMethod = List.isEmpty remainingMethods

            let argumentToTerm (index : int) (argument : methodSequenceArgument) =
                let passByRef, withIndex =
                    let parameterInfo =
                        if MethodSequenceHelpers.hasInstanceThis methodToCall then
                            if index = 0 then None
                            else methodToCall.Parameters[index - 1] |> Some
                        else methodToCall.Parameters[index] |> Some
                    match parameterInfo with
                    | None when MethodSequenceHelpers.isStruct methodToCall.DeclaringType ->
                        true, if isTargetMethod then Some -1 else None
                    | None ->
                        false, None
                    | Some pi ->
                        pi.ParameterType.IsByRef, if isTargetMethod then Some(-pi.Position - 2) else None

                match argument with
                | Variable({ typ = typ; index = varIndex } as varId) ->
                    if MethodSequenceHelpers.canBeCreatedBySolver typ then
                        let index =
                            match withIndex with
                            | Some newIndex ->
                                aliases.Add(varId, newIndex)
                                newIndex
                            | None -> varIndex
                        let name = $"arg_{typ.Name}_{index}"
                        let ref = Memory.AllocateTemporaryLocalVariableOfType newCoreState name index typ
                        if passByRef then ref else Memory.Read newCoreState ref
                    else
                        let key = TemporaryLocalVariableKey(typ, varIndex)
                        let key =
                            match withIndex with
                            | Some newIndex ->
                                aliases.Add(varId, newIndex)
                                let currentTerm = Memory.ReadLocalVariable newCoreState key
                                Memory.AllocateTemporaryLocalVariable newCoreState newIndex typ currentTerm |> ignore
                                TemporaryLocalVariableKey(typ, newIndex)
                            | None -> key
                        if passByRef then PrimitiveStackLocation key |> Ref else Memory.ReadLocalVariable newCoreState key
                | Default typ ->
                    assert(not <| Types.IsValueType typ)
                    let term = NullRef typ
                    match withIndex with
                    | None -> term
                    | Some idx ->
                        let ref = Memory.AllocateTemporaryLocalVariable newCoreState idx typ term
                        if passByRef then ref else Memory.Read newCoreState ref
                | Hole _ -> __unreachable__()

            let argumentTerms = MethodSequenceHelpers.thisAndArguments this arguments |> Seq.mapi argumentToTerm |> Seq.toList
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
