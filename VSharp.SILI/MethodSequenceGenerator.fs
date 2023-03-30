namespace VSharp.MethodSequences

open System
open System.Collections.Generic
open System.Reflection
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL

type internal methodSequenceState =
    {
        cilState : cilState
        isInMethod : bool
        methodsToCall : Method list
        currentSequence : methodSequenceElement list
        targetMethod : Method
    }

type internal methodSequenceSearcherResult =
    | NotExist
    | Exists
    | Unknown


type internal IMethodSequenceSearcher =
    abstract Pick : unit -> methodSequenceState option
    abstract Update : methodSequenceState -> methodSequenceState option
    abstract Add : methodSequenceState -> unit

type MethodSequenceSearcher() =
    interface IMethodSequenceSearcher with
        override x.Pick() = None
        member this.Add(var0) = ()
        member this.Update(var0) = failwith "todo"


type internal MethodSequenceGenerator(searcherFactory : unit -> IMethodSequenceSearcher, interpreter : ILInterpreter) =

    let searchers = Dictionary<Method, IMethodSequenceSearcher>()

    let isAvailableInPublicApi (method : Method) =
        method.IsPublic && (method.DeclaringType.IsPublic || method.DeclaringType.IsNestedPublic)

    let isPrimitive (t : Type) = t.IsPrimitive || t.IsEnum

    let canHaveDefaultThis (method : Method) =
        not method.HasThis || method.DeclaringType.IsValueType

    let makeSequenceForDefaultModel (cilState : cilState) =
        let targetMethod = CilStateOperations.entryMethodOf cilState
        if canHaveDefaultThis targetMethod then
            match cilState.state.model with
            | StateModel(currentModelState, currentTypeModel, None) ->
                let parameters = seq {
                    if targetMethod.HasThis then yield Default(targetMethod.DeclaringType)
                    yield! targetMethod.Parameters |> Array.map (fun pi -> Default pi.ParameterType)
                }
                let targetMethodCall = Call(targetMethod, None, parameters |> Seq.toList)
                cilState.state.model <- StateModel(currentModelState, currentTypeModel, Some [targetMethodCall])
                Exists
            | _ -> __unreachable__()
        else
            Unknown

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
                        if isPrimitive currentParameter.ParameterType ||
                           currentParameter.ParameterType.IsByRef && isPrimitive <| currentParameter.ParameterType.GetElementType() then
                            let argumentTerm =
                                if currentParameter.ParameterType.IsByRef then
                                    let key = ParameterKey currentParameter
                                    let argumentTerm = Memory.ReadLocalVariable cilState.state key
                                    Memory.Read modelState argumentTerm
                                else
                                    Memory.ReadArgument cilState.state currentParameter |> cilState.state.model.Complete
                            match argumentTerm with
                                | {term = Concrete(v, _)} ->
                                    readArgument remaining (Primitive(currentParameter.ParameterType, v) :: acc)
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


    let makeStep() : cilState list option =

        None

    // Parent is already changed!
    member x.GetSequenceOrEnqueue (cilState : cilState) =
        match cilState.state.model with
        | StateModel(_, _, Some _) ->
            Exists
        | _ ->
            let targetMethod = CilStateOperations.entryMethodOf cilState
            if not <| isAvailableInPublicApi targetMethod then NotExist
            elif tryConvertSolverModelToSequence cilState then Exists
            elif searchers.ContainsKey targetMethod then Unknown
            else
                // i. e. concrete int values + some null
                let initialCoreState = Memory.EmptyState()
                let baseMethod = Application.getMethod Loader.MethodSequenceBase
                let initialCilState = CilStateOperations.makeInitialState baseMethod initialCoreState
                // Add different constructors here?
                let initialState =
                    {
                        cilState = initialCilState
                        isInMethod = false
                        methodsToCall = List.empty
                        currentSequence = List.empty
                        targetMethod = targetMethod
                    }
                let searcher = searcherFactory()
                searcher.Add initialState
                searchers[targetMethod] <- searcher
                Unknown
