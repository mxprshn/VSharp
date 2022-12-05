namespace VSharp.Interpreter.IL

open System.Reflection
open VSharp
open VSharp.Core
open CilStateOperations

module StateInitialization =

    let private trySubstituteTypeParameters (methodBase : MethodBase) =
        let model = typeModel.CreateEmpty()
        let method = Application.getMethod methodBase
        let getConcreteType = function
        | ConcreteType t -> Some t
        | _ -> None
        match SolveGenericMethodParameters model method with
        | Some(classParams, methodParams) ->
            let classParams = classParams |> Array.choose getConcreteType
            let methodParams = methodParams |> Array.choose getConcreteType
            if classParams.Length = methodBase.DeclaringType.GetGenericArguments().Length &&
                (methodBase.IsConstructor || methodParams.Length = methodBase.GetGenericArguments().Length) then
                let declaringType = Reflection.concretizeTypeParameters methodBase.DeclaringType classParams
                let method = Reflection.concretizeMethodParameters declaringType methodBase methodParams
                method, model
            else
                methodBase, model
        | _ ->
            methodBase, model

    let private allocateByRefParameters initialState (method : Method) =
        let allocateIfByRef (pi : ParameterInfo) =
            if pi.ParameterType.IsByRef then
                if Memory.CallStackSize initialState = 0 then
                    Memory.NewStackFrame initialState None []
                let stackRef = Memory.AllocateTemporaryLocalVariableOfType initialState pi.Name (pi.Position + 1) (pi.ParameterType.GetElementType())
                Some stackRef
            else
                None
        method.Parameters |> Array.map allocateIfByRef |> Array.toList

    let private initializeIsolatedStatesPrivate (entryMethod : Method option) (currentLoc : codeLocation) (typeModel : typeModel) (isConcolic : bool) =
        let method = currentLoc.method
        let initialState = Memory.EmptyState()
        initialState.model <- Memory.EmptyModel currentLoc.method typeModel
        let cilState = makeCilState entryMethod currentLoc 0u initialState
        let this(*, isMethodOfStruct*) =
            if method.IsStatic then None // *TODO: use hasThis flag from Reflection
            else
                let this =
                    if Types.IsValueType method.DeclaringType then
                        Memory.NewStackFrame initialState None []
                        Memory.AllocateTemporaryLocalVariableOfType initialState "this" 0 method.DeclaringType
                    else
                        Memory.MakeSymbolicThis method
                !!(IsNullReference this) |> AddConstraint initialState
                Some this
        let parameters = allocateByRefParameters initialState method
        ILInterpreter.InitFunctionFrame initialState method this (Some parameters)
        let cilStates = ILInterpreter.CheckDisallowNullAssumptions cilState method false
        assert (List.length cilStates = 1)
        let [cilState] = cilStates
        if isConcolic then
            List.singleton cilState
        else
            ILInterpreter.InitializeStatics cilState method.DeclaringType List.singleton

    let private initializeMainMethodStatesPrivate (method : Method) (mainArguments : string[]) (typModel : typeModel) : cilState list =
        assert method.IsStatic
        let optionArgs = if mainArguments = null then None else Some mainArguments
        let state = Memory.EmptyState()
        state.model <- Memory.EmptyModel method typModel
        let argsToState args =
            let argTerms = Seq.map (fun str -> Memory.AllocateString str state) args
            let stringType = typeof<string>
            let argsNumber = MakeNumber mainArguments.Length
            Memory.AllocateConcreteVectorArray state argsNumber stringType argTerms
        let arguments = Option.map (argsToState >> Some >> List.singleton) optionArgs
        ILInterpreter.InitFunctionFrame state method None arguments
        if Option.isNone optionArgs then
            // NOTE: if args are symbolic, constraint 'args != null' is added
            let parameters = method.Parameters
            assert(Array.length parameters = 1)
            let argsParameter = Array.head parameters
            let argsParameterTerm = Memory.ReadArgument state argsParameter
            AddConstraint state (!!(IsNullReference argsParameterTerm))
        Memory.InitializeStaticMembers state method.DeclaringType
        let initialState = makeCilState (Some method) { method = method; offset = 0<offsets> } 0u state
        [initialState]

    let initializeIsolatedMethodStates (method : MethodBase) (isConcolic : bool) =
        let method, typeModel = trySubstituteTypeParameters method
        let method = Application.getMethod method
        let loc = { method = method; offset = 0<offsets> }
        method, initializeIsolatedStatesPrivate (Some method) loc typeModel isConcolic

    let initializeMainMethodStates (method : MethodBase) (args : string[]) =
        let method, typeModel = trySubstituteTypeParameters method
        let method = Application.getMethod method
        method, initializeMainMethodStatesPrivate method args typeModel

    let initializeIsolatedStates (startingLocation : codeLocation) =
        initializeIsolatedStatesPrivate None startingLocation (typeModel.CreateEmpty()) false
