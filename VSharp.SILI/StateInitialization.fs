namespace VSharp.Interpreter.IL

open System.Collections.Generic
open System.Reflection
open VSharp
open VSharp.Core
open CilStateOperations

type IStateInitializer =
    abstract member InitializeIsolatedMethodStates : MethodBase -> Method * cilState list
    abstract member InitializeMainMethodStates : MethodBase -> string[] -> Method * cilState list
    abstract member InitializeIsolatedStates : codeLocation -> cilState list
    abstract member Reset : unit -> unit

type StateInitializer(isConcolicMode : bool) =

    let initializedIsolatedMethods = Dictionary<Method, typeModel>()
    let initializedMainMethods = Dictionary<Method, typeModel * string[]>()

    let trySubstituteTypeParameters (methodBase : MethodBase) =
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

    let allocateByRefParameters initialState (method : Method) =
        let allocateIfByRef (pi : ParameterInfo) =
            if pi.ParameterType.IsByRef then
                if Memory.CallStackSize initialState = 0 then
                    Memory.NewStackFrame initialState None []
                let stackRef = Memory.AllocateTemporaryLocalVariableOfType initialState pi.Name (pi.Position + 1) (pi.ParameterType.GetElementType())
                Some stackRef
            else
                None
        method.Parameters |> Array.map allocateIfByRef |> Array.toList

    let initializeIsolatedStates (entryMethod : Method option) (currentLoc : codeLocation) (typeModel : typeModel) =
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
        if isConcolicMode then
            List.singleton cilState
        else
            ILInterpreter.InitializeStatics cilState method.DeclaringType List.singleton

    let initializeMainMethodStates (method : Method) (mainArguments : string[]) (typModel : typeModel) : cilState list =
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

    interface IStateInitializer with
        override x.InitializeIsolatedMethodStates (method : MethodBase) =
            let method, typeModel = trySubstituteTypeParameters method
            let method = Application.getMethod method
            let loc = { method = method; offset = 0<offsets> }
            initializedIsolatedMethods.[method] <- typeModel
            method, initializeIsolatedStates (Some method) loc typeModel

        override x.InitializeMainMethodStates (method : MethodBase) (args : string[]) =
            let method, typeModel = trySubstituteTypeParameters method
            let method = Application.getMethod method
            initializedMainMethods.[method] <- (typeModel, args)
            method, initializeMainMethodStates method args typeModel

        override x.InitializeIsolatedStates (startingLocation : codeLocation) =
            let isOnMethodStart = startingLocation.offset = 0<offsets>
            if isOnMethodStart && initializedMainMethods.ContainsKey startingLocation.method then
                let typeModel, args = initializedMainMethods.[startingLocation.method]
                initializeMainMethodStates startingLocation.method args typeModel
            elif isOnMethodStart && initializedIsolatedMethods.ContainsKey startingLocation.method then
                let typeModel = initializedIsolatedMethods.[startingLocation.method]
                initializeIsolatedStates (Some startingLocation.method) startingLocation typeModel
            else
                initializeIsolatedStates None startingLocation (typeModel.CreateEmpty())

        override x.Reset() =
            initializedIsolatedMethods.Clear()
            initializedMainMethods.Clear()
