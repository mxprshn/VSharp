namespace VSharp.Interpreter.IL

open System
open System.Collections.Generic
open System.Reflection
open FSharpx.Collections
open VSharp
open VSharp.Core
open System.Linq
open VSharp.MethodSequences

type testGeneratorCache = {
    indices : Dictionary<concreteHeapAddress, int>
    methodSequenceObjects : Dictionary<variableId, obj>
    mockCache : Dictionary<ITypeMock, Mocking.Type>
    implementations : IDictionary<MethodInfo, term[]>
}

type public testSuite =
    | Test
    | Error of string * bool
    with
    member x.IsErrorSuite with get() =
        match x with
        | Error _ -> true
        | Test -> false
    member x.IsFatalError with get() =
        match x with
        | Error(_, isFatal) -> isFatal
        | Test -> false
    member x.ErrorMessage with get() =
        match x with
        | Error(msg, _) -> msg
        | Test -> null

module TestGenerator =

    let mutable private maxBufferSize = 128
    let public setMaxBufferSize size = maxBufferSize <- size

    let private addMockToMemoryGraph (indices : Dictionary<concreteHeapAddress, int>) encodeMock evalField (test : UnitTest) addr (mock : ITypeMock) =
        let index = test.MemoryGraph.ReserveRepresentation()
        indices.Add(addr, index)
        let mock : Mocking.Type = encodeMock mock
        let baseClass = mock.BaseClass
        let fields =
            match evalField with
            | Some evalField when baseClass <> null && not (TypeUtils.isDelegate baseClass) ->
                Reflection.fieldsOf false baseClass
                |> Array.map (fst >> evalField)
            | _ -> Array.empty
        test.MemoryGraph.AddMockedClass mock fields index :> obj

    let private obj2test eval encodeArr (cache : testGeneratorCache) encodeMock (test : UnitTest) addr typ (methodSequenceObjRef : obj) =
        let index = ref 0
        let indices = cache.indices
        if indices.TryGetValue(addr, index) then
            let referenceRepr : referenceRepr = {index = index.Value}
            referenceRepr :> obj
        else
            let memoryGraph = test.MemoryGraph
            let cha = ConcreteHeapAddress addr
            match typ with
            | ConcreteType typ when TypeUtils.isDelegate typ ->
                // Obj is a delegate which mock hasn't been created yet
                let mock = TypeMock(Seq.singleton typ)
                addMockToMemoryGraph indices encodeMock None test addr mock
            | ConcreteType typ ->
                match typ with
                | TypeUtils.ArrayType(elemType, dim) ->
                    let index = memoryGraph.ReserveRepresentation()
                    indices.Add(addr, index)
                    let arrayType, (lengths : int array), (lowerBounds : int array) =
                        match dim with
                        | Vector ->
                            let arrayType = (elemType, 1, true)
                            let len = ArrayLength(cha, MakeNumber 0, arrayType) |> eval |> unbox
                            arrayType, [| len |], null
                        | ConcreteDimension rank ->
                            let arrayType = (elemType, rank, false)
                            arrayType,
                            Array.init rank (fun i -> ArrayLength(cha, MakeNumber i, arrayType) |> eval |> unbox),
                            Array.init rank (fun i -> ArrayLowerBound(cha, MakeNumber i, arrayType) |> eval |> unbox)
                        | SymbolicDimension -> __notImplemented__()
                    let length = Array.reduce (*) lengths
                    // TODO: normalize model (for example, try to minimize lengths of generated arrays)
                    if maxBufferSize > 0 && length > maxBufferSize then
                        raise <| InsufficientInformationException "Test generation for too large buffers disabled for now"
                    let repr = encodeArr test arrayType addr typ lengths lowerBounds index
                    repr :> obj
                | _ when typ.IsValueType ->
                    let index = memoryGraph.ReserveRepresentation()
                    indices.Add(addr, index)
                    let content = BoxedLocation(ConcreteHeapAddress addr, typ) |> eval
                    let repr = memoryGraph.AddBoxed content index
                    repr :> obj
                | _ when typ = typeof<string> ->
                    let index = memoryGraph.ReserveRepresentation()
                    indices.Add(addr, index)
                    let length : int = ClassField(cha, Reflection.stringLengthField) |> eval |> unbox
                    let string =
                        if length <= 0 then String.Empty
                        else
                            let readChar i =
                                ArrayIndex(cha, [MakeNumber i], (typeof<char>, 1, true)) |> eval |> unbox
                            let contents : char array = Array.init length readChar
                            String(contents)
                    memoryGraph.AddString index string
                | _ ->
                    let index = memoryGraph.ReserveRepresentation()
                    indices.Add(addr, index)
                    let fields = typ |> Reflection.fieldsOf false |> Array.map (fun (field, _) ->
                        ClassField(cha, field) |> eval)
                    let repr = memoryGraph.AddClass typ fields index methodSequenceObjRef
                    repr :> obj
            | MockType mock when mock.IsValueType -> memoryGraph.RepresentMockedStruct (encodeMock mock) Array.empty
            | MockType mock ->
                let evalField field = ClassField(cha, field) |> eval
                addMockToMemoryGraph indices encodeMock (Some evalField) test addr mock

    let private encodeArrayCompactly (state : state) (model : model) (encode : term -> obj) (test : UnitTest) arrayType cha typ lengths lowerBounds index =
        if state.concreteMemory.Contains cha then
            // TODO: Use compact representation for big arrays
            let contents =
                state.concreteMemory.VirtToPhys cha :?> Array
                |> Array.mapToOneDArray test.MemoryGraph.Encode
            test.MemoryGraph.AddArray typ contents lengths lowerBounds index
        else
            // Solver may return models with unused values with indices out of
            // legal range. This code throws such values away to avoid IndexOutOfRange's
            // while running tests.
            let checkArrayIndex =
                if lowerBounds = null then
                    (fun (i : int list) ->
                        assert(lengths.Length = 1 && i.Length = 1)
                        List.head i < lengths[0])
                else
                    List.toSeq >> Seq.zip3 lowerBounds lengths >> Seq.forall (fun (lb, l, i) -> i >= lb && i < lb + l)
            // TODO: move resolving to some model interface
            let arrayRegion =
                if VectorTime.less cha VectorTime.zero then
                    match model with
                    | StateModel(modelState, _) ->
                        match PersistentDict.tryFind modelState.arrays arrayType with
                        | Some _ as region -> region
                        | None ->
                            match modelState.model with
                            | StateModel(internalModel, _) ->
                                PersistentDict.tryFind internalModel.arrays arrayType
                            | _ -> None
                    | _ -> __unreachable__()
                else
                    PersistentDict.tryFind state.arrays arrayType

            let defaultValue, indices, values =
                match arrayRegion with
                | Some region ->
                    let defaultValue =
                        match region.defaultValue with
                        | Some defaultValue -> encode defaultValue
                        | None -> null
                    let updates = region.updates
                    let indicesWithValues = SortedDictionary<int list, obj>()
                    let addOneKey _ (k : updateTreeKey<heapArrayKey, term>) () =
                        let value = k.value
                        match k.key with
                        | OneArrayIndexKey(address, keyIndices) ->
                            let heapAddress = model.Eval address
                            match heapAddress with
                            | {term = ConcreteHeapAddress(cha')} when cha' = cha ->
                                let i = keyIndices |> List.map (encode >> unbox)
                                let v = encode value
                                if checkArrayIndex i then
                                    indicesWithValues[i] <- v
                            | _ -> ()
                        | RangeArrayIndexKey(address, fromIndices, toIndices) ->
                            let heapAddress = model.Eval address
                            match heapAddress with
                            | {term = ConcreteHeapAddress(cha')} when cha' = cha ->
                                let fromIndices : int list = fromIndices |> List.map (encode >> unbox)
                                let toIndices : int list = toIndices |> List.map (encode >> unbox)
                                let allIndices = Array.allIndicesViaBound fromIndices toIndices
                                match value.term with
                                | Constant(_, ArrayRangeReading, _) ->
                                    for i in allIndices do
                                        let index = List.map MakeNumber i
                                        let key = OneArrayIndexKey(heapAddress, index)
                                        let v = SpecializeWithKey value key k.key |> encode
                                        if checkArrayIndex i then
                                            indicesWithValues[i] <- v
                                | _ ->
                                    for i in allIndices do
                                        let v = encode value
                                        if checkArrayIndex i then
                                            indicesWithValues[i] <- v
                            | _ -> ()
                    updates |> RegionTree.foldr addOneKey ()
                    let indices = indicesWithValues.Keys.ToArray()
                    let values = indicesWithValues.Values.ToArray()
                    defaultValue, indices.ToArray(), values.ToArray()
                | None -> null, Array.empty, Array.empty
            let indices = Array.map Array.ofList indices
            test.MemoryGraph.AddCompactArrayRepresentation typ defaultValue indices values lengths lowerBounds index

    let rec private term2obj (model : model) state (cache : testGeneratorCache) (test : UnitTest) (methodSequenceObjRef : obj) (term : term) =
        let term2obj = term2obj model state cache test
        match term with
        | {term = Concrete(_, TypeUtils.AddressType)} -> __unreachable__()
        | {term = Concrete(v, t)} when t = typeof<IntPtr> ->
            test.MemoryGraph.RepresentIntPtr (int64 (v :?> IntPtr))
        | {term = Concrete(v, t)} when t = typeof<UIntPtr> ->
            test.MemoryGraph.RepresentUIntPtr (int64 (v :?> UIntPtr))
        | {term = Concrete(v, t)} when t.IsEnum -> test.MemoryGraph.RepresentEnum v
        | {term = Concrete(v, _)} -> v
        | {term = Nop} -> null
        | {term = Constant _ } as c -> model.Eval c |> term2obj methodSequenceObjRef
        | {term = Struct(fields, t)} when Types.IsNullable t ->
            let valueField, hasValueField = Reflection.fieldsOfNullable t
            let hasValue : bool = term2obj null fields[hasValueField] |> unbox
            if hasValue then
                term2obj null fields[valueField]
            else null
        | {term = Struct(fields, t)} as term ->
            let methodSequenceObjRef =
                if methodSequenceObjRef <> null then methodSequenceObjRef
                else
                    let defaultStruct = Memory.DefaultOf t
                    if term = defaultStruct
                    then
                        let structCtorRef = test.MemoryGraph.AddMethodSequenceStructCtor t
                        test.MemoryGraph.AddMethodSequence [|structCtorRef|]
                        structCtorRef.resultObj :> obj
                    else null
            let fieldReprs =
                t |> Reflection.fieldsOf false |>
                    Array.map (fun (field, _) -> model.Eval fields.[field] |> model.Complete |> term2obj null)
            test.MemoryGraph.RepresentStruct t fieldReprs methodSequenceObjRef
        | NullRef _
        | NullPtr -> null
        | DetachedPtrTerm offset ->
            let offset = TypeUtils.convert (term2obj offset) typeof<int64> :?> int64
            test.MemoryGraph.RepresentDetachedPtr typeof<Void> offset
        | {term = Ptr(HeapLocation({term = ConcreteHeapAddress(addr)}, _), sightType, offset)} ->
            let offset = TypeUtils.convert (term2obj offset) typeof<int64> :?> int64
            let obj = address2obj model state cache test methodSequenceObjRef addr
            if obj :? referenceRepr then
                let index = (obj :?> referenceRepr).index
                test.MemoryGraph.RepresentPtr index sightType offset
            else test.MemoryGraph.RepresentDetachedPtr sightType offset
        | {term = HeapRef({term = ConcreteHeapAddress(addr)}, _)} ->
            address2obj model state cache test methodSequenceObjRef addr
        | CombinedTerm(terms, t) ->
            let slices = List.map model.Eval terms
            ReinterpretConcretes slices t
        | term -> internalfailf $"Creating object from term: unexpected term {term}"

    and private address2obj (model : model) state cache (test : UnitTest) (methodSequenceObjRef : obj) (address : concreteHeapAddress) : obj =
        let term2obj = term2obj model state cache test
        match address with
        // TODO: move to function
        | _ when VectorTime.less address state.startingTime ->
            match model with
            | StateModel(modelState, _) ->
                let allocatedType =
                    match PersistentDict.tryFind modelState.allocatedTypes address with
                    | Some _ as res -> res
                    | None ->
                        match modelState.model with
                        | StateModel(state, _) ->
                            PersistentDict.tryFind state.allocatedTypes address
                        | _ -> None
                match allocatedType with
                | Some typ ->
                    let eval address =
                        let stateToRead =
                            match modelState.model with
                            | StateModel(internalModel, _) ->
                                match address with
                                // TODO: rough workaround, think about encapsulating this logic into model
                                | ArrayIndex({ term = ConcreteHeapAddress arrayAddr }, _, _)
                                | ArrayLowerBound({ term = ConcreteHeapAddress arrayAddr }, _, _)
                                | ArrayLength({ term = ConcreteHeapAddress arrayAddr }, _, _) when PersistentDict.contains arrayAddr internalModel.allocatedTypes ->
                                    internalModel
                                | ClassField({ term = ConcreteHeapAddress arrayAddr }, fld) when Reflection.stringLengthField = fld && PersistentDict.contains arrayAddr internalModel.allocatedTypes ->
                                    internalModel
                                | _ -> modelState
                            | _ ->
                                modelState
                        address |> Ref |> Memory.Read stateToRead |> model.Complete |> term2obj null
                    let arr2Obj = encodeArrayCompactly state model (term2obj null)
                    let encodeMock = encodeTypeMock model state cache test
                    obj2test eval arr2Obj cache encodeMock test address typ methodSequenceObjRef
                // If address is not in the 'allocatedTypes', it should not be allocated, so result is 'null'
                | None -> null
            | PrimitiveModel _ -> __unreachable__()
        | _ ->
            let term2Obj = model.Eval >> term2obj null
            let eval address = Ref address |> Memory.Read state |> term2Obj
            let arr2Obj = encodeArrayCompactly state model term2Obj
            let typ = state.allocatedTypes[address]
            let encodeMock = encodeTypeMock model state cache test
            obj2test eval arr2Obj cache encodeMock test address typ methodSequenceObjRef

    and private encodeTypeMock (model : model) state (cache : testGeneratorCache) (test : UnitTest) mock : Mocking.Type =
        let mockedType = ref Mocking.Type.Empty
        if cache.mockCache.TryGetValue(mock, mockedType) then mockedType.Value
        else
            let eval = model.Eval >> term2obj model state cache test null
            let freshMock = Mocking.Type(mock.Name)
            cache.mockCache.Add(mock, freshMock)
            for t in mock.SuperTypes do
                freshMock.AddSuperType t
                for methodMock in cache.implementations do
                    let method = methodMock.Key
                    let values = methodMock.Value
                    let methodType = method.ReflectedType
                    let mockedBaseInterface() =
                        methodType.IsInterface && Seq.contains methodType (TypeUtils.getBaseInterfaces t)
                    if methodType = t || mockedBaseInterface() then
                        freshMock.AddMethod(method, Array.map eval values)
            freshMock

    let encodeExternMock (model : model) state (cache : testGeneratorCache) test (methodMock : IMethodMock) =
        let eval = model.Eval >> term2obj model state cache test null
        let clauses = methodMock.GetImplementationClauses() |> Array.map eval
        let extMock = extMockRepr.Encode test.GetPatchId methodMock.BaseMethod clauses
        test.AddExternMock extMock

    let encodeMethodSequence (model : model) state cache modelState (sequence : methodSequence) test =
        let encodeArg (methodSequenceArgument : methodSequenceArgument) : obj =
            match methodSequenceArgument with
            | Default _ -> null
            | Variable({ typ = typ; index = index } as varId) ->
                let objRef = ref null
                if cache.methodSequenceObjects.TryGetValue(varId, objRef) then
                    objRef.Value
                else
                    let key = TemporaryLocalVariableKey(typ, index)
                    let term = Memory.ReadLocalVariable modelState key |> model.Complete
                    term2obj model state cache test null term
            | Hole _ -> failwith "Can't encode method sequence with hole"
        let encodeMethodSequenceElement (element : methodSequenceElement) : obj =
            match element with
            | Call(method, resultVar, this, args) ->
                let thisRepr =
                    match this with
                    | Some thisArg -> encodeArg thisArg
                    | None -> null
                let argReprs = args |> List.map encodeArg |> List.toArray
                let hasNonVoidResult = resultVar.IsSome
                let callRepr = test.MemoryGraph.AddMethodSequenceCall method thisRepr argReprs hasNonVoidResult
                if hasNonVoidResult
                then cache.methodSequenceObjects[resultVar.Value] <- callRepr.resultObj
                callRepr
            | CreateDefaultStruct resultVar ->
                let structCtorRepr = test.MemoryGraph.AddMethodSequenceStructCtor resultVar.typ
                cache.methodSequenceObjects[resultVar] <- structCtorRepr.resultObj
                structCtorRepr
        let elementReprs = sequence.sequence |> List.map encodeMethodSequenceElement |> List.toArray
        test.MemoryGraph.AddMethodSequence elementReprs

    let private modelState2test (test : UnitTest) suite cache (m : Method) model modelState methodSequence (state : state) =
        match SolveGenericMethodParameters state.typeStorage m with
        | None -> None
        | Some(classParams, methodParams) ->
            let thisMethodSequenceRef, argMethodSequenceRefs =
                match methodSequence with
                | Some(sequence) ->
                    test.HasMethodSequence <- true
                    if (not <| List.isEmpty sequence.sequence) then
                        encodeMethodSequence model state cache modelState sequence test
                        let thisMethodSequenceRef =
                            match sequence.this with
                            | Some thisId -> cache.methodSequenceObjects[thisId]
                            | None -> null
                        let argMethodSequenceRefs = Dictionary<ParameterInfo, obj>()
                        for pi, argId in PersistentDict.toSeq sequence.args do
                            let objRef = ref null
                            if cache.methodSequenceObjects.TryGetValue(argId, objRef)
                            then
                                argMethodSequenceRefs[pi] <- objRef.Value
                        thisMethodSequenceRef, argMethodSequenceRefs
                    else
                        null, Dictionary()
                | _ -> null, Dictionary()


            for entry in state.methodMocks do
                let mock = entry.Value
                match mock.MockingType with
                | MockingType.Default ->
                    let values = mock.GetImplementationClauses()
                    cache.implementations.Add(mock.BaseMethod, values)
                | Extern ->
                    encodeExternMock model state cache test mock

            let concreteClassParams = Array.zeroCreate classParams.Length
            let mockedClassParams = Array.zeroCreate classParams.Length
            let concreteMethodParams = Array.zeroCreate methodParams.Length
            let mockedMethodParams = Array.zeroCreate methodParams.Length
            let encodeMock = encodeTypeMock model state cache test
            let processSymbolicType (concreteArr : Type array) (mockArr : Mocking.Type option array) i = function
                | ConcreteType t -> concreteArr[i] <- t
                | MockType m -> mockArr[i] <- Some (encodeMock m)
            classParams |> Seq.iteri (processSymbolicType concreteClassParams mockedClassParams)
            methodParams |> Seq.iteri (processSymbolicType concreteMethodParams mockedMethodParams)
            test.SetTypeGenericParameters concreteClassParams mockedClassParams
            test.SetMethodGenericParameters concreteMethodParams mockedMethodParams

            let parametersInfo = m.Parameters
            if state.complete then
                for pi in parametersInfo do
                    let arg = Memory.ReadArgument state pi
                    let concreteArg = term2obj model state cache test (argMethodSequenceRefs.GetValueOrDefault(pi)) arg
                    test.AddArg (Array.head parametersInfo) concreteArg
            else
                for pi in parametersInfo do
                    let value =
                        if pi.ParameterType.IsByRef then
                            let key = ParameterKey pi
                            let stackRef = Memory.ReadLocalVariable state key
                            Memory.Read modelState stackRef
                        else
                            Memory.ReadArgument modelState pi |> model.Eval |> model.Complete
                    let concreteValue : obj = term2obj model state cache test (argMethodSequenceRefs.GetValueOrDefault(pi)) value
                    test.AddArg pi concreteValue

            if m.HasThis then
                let thisTerm =
                    if m.DeclaringType.IsValueType then
                        let stackRef = Memory.ReadThis state m
                        Memory.Read modelState stackRef
                    else
                        Memory.ReadThis modelState m |> model.Eval |> model.Complete
                let concreteThis = term2obj model state cache test thisMethodSequenceRef thisTerm
                test.ThisArg <- concreteThis

            match state.exceptionsRegister, suite with
            | Unhandled(e, _, _), Error(msg, isFatal) ->
                test.Exception <- MostConcreteTypeOfRef state e
                test.IsError <- true
                if String.IsNullOrEmpty msg then
                    let messageReference = Memory.ReadField state e Reflection.exceptionMessageField |> model.Eval
                    let encoded = term2obj model state cache test null messageReference
                    test.ErrorMessage <- test.MemoryGraph.DecodeString encoded
                else test.ErrorMessage <- msg
                test.IsFatalError <- isFatal
            | Unhandled(e, _, _), _ ->
                test.Exception <- MostConcreteTypeOfRef state e
            | _, Error(msg, isFatal) ->
                test.IsError <- true
                test.ErrorMessage <- msg
                test.IsFatalError <- isFatal
            | _ ->
                let retVal = Memory.StateResult state |> model.Eval
                test.Expected <- term2obj model state cache test null retVal
            Some test

    let private model2test (test : UnitTest) suite cache (m : Method) model (state : state) =
        let suitableState state =
            if m.HasParameterOnStack then Memory.CallStackSize state = 2
            else Memory.CallStackSize state = 1

        if not <| suitableState state then
            internalfail "Finished state has many frames on stack! (possibly unhandled exception)"

        match model with
        | StateModel(modelState, methodSequence) -> modelState2test test suite cache m model modelState methodSequence state
        | _ -> __unreachable__()

    let public state2test testSuite (m : Method) (state : state) =
        let test = UnitTest((m :> IMethod).MethodBase)
        let cache =
            {
                indices = Dictionary<concreteHeapAddress, int>()
                mockCache = Dictionary<ITypeMock, Mocking.Type>()
                implementations = Dictionary<MethodInfo, term[]>()
                methodSequenceObjects = Dictionary<variableId, obj>()
            }
        model2test test testSuite cache m state.model state

    let public state2testWithMockingCache testSuite (m : Method) (state : state) mockCache =
        let test = UnitTest((m :> IMethod).MethodBase)
        let cache =
            {
                indices = Dictionary<concreteHeapAddress, int>()
                mockCache = mockCache
                implementations = Dictionary<MethodInfo, term[]>()
                methodSequenceObjects = Dictionary<variableId, obj>()
            }
        model2test test testSuite cache m state.model state
