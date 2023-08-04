namespace VSharp

open System
open System.Collections.Generic
open System.Diagnostics
open System.Reflection
open System.Xml.Serialization
open Microsoft.FSharp.Collections

open VSharp
open VSharp.MethodSequences


[<CLIMutable>]
[<Serializable>]
type typeRepr = {
    assemblyName : string
    moduleFullyQualifiedName : string
    name : string
    genericArgs : typeRepr array
}
with
    static member Encode(t : Type) =
        if t = null then
            {
                assemblyName = null
                moduleFullyQualifiedName = null
                name = null
                genericArgs = null
            }
        else
            let name, arguments =
                if t.IsGenericType then
                    if not t.IsConstructedGenericType then
                        internalfail "Encoding not constructed generic types not supported"

                    let arguments =
                        t.GetGenericArguments()
                        |> Seq.map typeRepr.Encode
                        |> Seq.toArray
                    let name = t.GetGenericTypeDefinition().FullName
                    name, arguments
                else
                    t.FullName, null

            {
                assemblyName = t.Module.Assembly.FullName
                moduleFullyQualifiedName = t.Module.FullyQualifiedName
                name = name
                genericArgs = arguments
            }

    member x.Decode() =
        let rec decodeTypeRec (t : typeRepr) =
            let mdle = Reflection.resolveModule t.assemblyName t.moduleFullyQualifiedName
            let typ = mdle.GetType t.name
            Debug.Assert(typ <> null)

            if typ.IsGenericType then
                Debug.Assert(t.genericArgs <> null && typ.GetGenericArguments().Length = t.genericArgs.Length)

                let args = t.genericArgs |> Seq.map decodeTypeRec |> Seq.toArray
                typ.MakeGenericType args
            else
                typ
        if x.assemblyName = null then
            null
        else
            let decodedType = decodeTypeRec x
            AssemblyManager.NormalizeType decodedType

[<CLIMutable>]
[<Serializable>]
type methodRepr = {
    declaringType : typeRepr
    token : int
}
with
    static member Encode(m : MethodBase) : methodRepr =
        {
            declaringType = typeRepr.Encode m.DeclaringType
            token = m.MetadataToken
        }

    static member Encode(m : IMethod) : methodRepr =
        {
            declaringType = typeRepr.Encode m.DeclaringType
            token = m.MetadataToken
        }

    member x.Decode() : MethodBase =
        let declaringType = x.declaringType.Decode()
        match declaringType.GetMethods() |> Seq.tryFind (fun m -> m.MetadataToken = x.token) with
        | Some m -> m
        | None -> declaringType.GetConstructors() |> Seq.find (fun c -> c.MetadataToken = x.token) :> MethodBase

    member x.DecodeMethodInfo() : MethodInfo =
        let declaringType = x.declaringType.Decode()
        declaringType.GetMethods() |> Seq.find (fun m -> m.MetadataToken = x.token)

[<CLIMutable>]
[<Serializable>]
type referenceRepr = {
    index : int
}

[<CLIMutable>]
[<Serializable>]
type enumRepr = {
    typ : int
    underlyingValue : obj
}

[<CLIMutable>]
[<Serializable>]
type stringRepr = {
    content : string
}
with
    static member Encode(str : string) : stringRepr =
        { content = Text.Encoding.UTF8.GetBytes str |> Convert.ToBase64String }

    member x.Decode() =
        Convert.FromBase64String x.content |> Text.Encoding.UTF8.GetString

[<CLIMutable>]
[<Serializable>]
type pointerRepr = {
    index : int
    shift : int64
    sightType : int
}

[<CLIMutable>]
[<Serializable>]
[<XmlInclude(typeof<structureRepr>)>]
[<XmlInclude(typeof<referenceRepr>)>]
[<XmlInclude(typeof<pointerRepr>)>]
[<XmlInclude(typeof<enumRepr>)>]
[<XmlInclude(typeof<stringRepr>)>]
type structureRepr = {
    typ : int
    fields : obj array
    // To represent objects in memory as well as method sequence result simultaneously
    methodSequenceRef : obj
}

[<CLIMutable>]
[<Serializable>]
[<XmlInclude(typeof<structureRepr>)>]
[<XmlInclude(typeof<referenceRepr>)>]
[<XmlInclude(typeof<pointerRepr>)>]
[<XmlInclude(typeof<enumRepr>)>]
[<XmlInclude(typeof<stringRepr>)>]
// If indices = null, then values is just the whole content of an array.
// Otherwise, indices.Length = values.Length is guaranteed, and the array can be decoded by filling
//    the whole array with defaultValue and then synchronously writing values into indices
type arrayRepr = {
    typ : int
    defaultValue : obj
    indices : int array array
    values : obj array
    lengths : int array
    lowerBounds : int array
}

[<CLIMutable>]
[<Serializable>]
[<XmlInclude(typeof<structureRepr>)>]
[<XmlInclude(typeof<referenceRepr>)>]
[<XmlInclude(typeof<pointerRepr>)>]
[<XmlInclude(typeof<arrayRepr>)>]
[<XmlInclude(typeof<enumRepr>)>]
[<XmlInclude(typeof<methodRepr>)>]
[<XmlInclude(typeof<stringRepr>)>]
type typeMockRepr = {
    name : string
    baseClass : typeRepr
    interfaces : typeRepr array
    baseMethods : methodRepr array
    methodImplementations : obj array array
}
with
    static member NullRepr =
        {
            name = null
            baseClass = typeRepr.Encode null
            interfaces = [||]
            baseMethods = [||]
            methodImplementations = [||]
        }

    static member Encode (t : Mocking.Type) (encode : obj -> obj) =
        {
            name = t.Id
            baseClass = typeRepr.Encode t.BaseClass
            interfaces =
                t.Interfaces |> Seq.map typeRepr.Encode |> Array.ofSeq
            baseMethods =
                t.MethodMocks |> Seq.map (fun m -> methodRepr.Encode m.BaseMethod) |> Array.ofSeq
            methodImplementations =
                t.MethodMocks |> Seq.map (fun m -> m.ReturnValues |> Array.map encode) |> Array.ofSeq
        }

    member x.Decode() =
        let baseClass = x.baseClass.Decode()
        let interfaces = x.interfaces |> Array.map (fun i -> i.Decode())
        let baseMethods = x.baseMethods |> Array.map (fun m -> m.Decode() :?> MethodInfo)
        Mocking.Type.Deserialize x.name baseClass interfaces baseMethods x.methodImplementations

[<CLIMutable>]
[<Serializable>]
[<XmlInclude(typeof<structureRepr>)>]
[<XmlInclude(typeof<referenceRepr>)>]
[<XmlInclude(typeof<pointerRepr>)>]
[<XmlInclude(typeof<arrayRepr>)>]
[<XmlInclude(typeof<enumRepr>)>]
[<XmlInclude(typeof<methodRepr>)>]
type extMockRepr = {
    name : string
    baseMethod : methodRepr
    methodImplementation : obj array
}
with
    static member Encode name (baseMethod : MethodBase) results =
        {
            name = name
            baseMethod = methodRepr.Encode baseMethod
            methodImplementation = results
        }

    member x.Decode() =
        let baseMethod = x.baseMethod.DecodeMethodInfo()
        ExtMocking.Type.Deserialize x.name baseMethod x.methodImplementation

[<CLIMutable>]
[<Serializable>]
[<XmlInclude(typeof<structureRepr>)>]
[<XmlInclude(typeof<referenceRepr>)>]
[<XmlInclude(typeof<pointerRepr>)>]
[<XmlInclude(typeof<arrayRepr>)>]
[<XmlInclude(typeof<enumRepr>)>]
[<XmlInclude(typeof<typeMockRepr>)>]
[<XmlInclude(typeof<stringRepr>)>]
type memoryRepr = {
    objects : obj array
    types : typeRepr array
}

[<CLIMutable>]
[<Serializable>]
[<XmlInclude(typeof<structureRepr>)>]
[<XmlInclude(typeof<referenceRepr>)>]
[<XmlInclude(typeof<pointerRepr>)>]
[<XmlInclude(typeof<arrayRepr>)>]
[<XmlInclude(typeof<enumRepr>)>]
[<XmlInclude(typeof<typeMockRepr>)>]
type methodSequenceCallRepr = {
    methodRepr : methodRepr
    resultObj : obj
    thisArg : obj
    args : obj array
}

[<CLIMutable>]
[<Serializable>]
[<XmlInclude(typeof<referenceRepr>)>]
type methodSequenceStructCtorRepr = {
    typ : int
    resultObj : referenceRepr
}

[<CLIMutable>]
[<Serializable>]
[<XmlInclude(typeof<methodSequenceCallRepr>)>]
[<XmlInclude(typeof<methodSequenceStructCtorRepr>)>]
type methodSequenceRepr = {
    elements : obj array
}

type public CompactArrayRepr = {
    array : Array
    defaultValue : obj
    indices : int array array
    values : obj array
}

type MockStorage() =
    let mocker = Mocking.Mocker()
    let mockedTypes = List<Mocking.Type>()

    member x.Deserialize(typeMocks : typeMockRepr array) =
        typeMocks
        |> Array.map (fun r -> r.Decode())
        |> mockedTypes.AddRange

    member x.RegisterMockedType (typ : Mocking.Type) =
        match mockedTypes |> Seq.tryFindIndex ((=) typ) with
        | Some idx -> -idx - 1
        | None ->
            mockedTypes.Add(typ)
            -mockedTypes.Count

    member x.Item(index : int) : Mocking.Type * Type =
        let mockedType = mockedTypes[-index - 1]
        mockedType, mocker.BuildDynamicType mockedType

    member x.TypeMocks
        with get() = mockedTypes

type MemoryGraph(repr : memoryRepr, sequenceReprs : methodSequenceRepr array, mockStorage : MockStorage, createCompactRepr : bool, createMethodSequences : bool) =

    let sourceTypes = List<Type>(repr.types |> Array.map (fun t -> t.Decode()))
    let compactRepresentations = Dictionary<obj, CompactArrayRepr>()
    let boxedLocations = HashSet<physicalAddress>()

    let intPtrIndex = Int32.MaxValue
    let uintPtrIndex = Int32.MinValue

    let methodSequenceReprs = List(sequenceReprs)
    let methodSequenceObjectIndices = Dictionary<obj, int>()
    let invokableMethodSequences = List<InvokableMethodSequence>()
    let mutable methodSequenceObjectCounter = 0

    let createMockObject decode index =
        let mockType, t = mockStorage[index]
        mockType.EnsureInitialized decode t
        let baseClass = mockType.BaseClass
        if TypeUtils.isDelegate baseClass then Mocking.Mocker.CreateDelegate baseClass t
        else Reflection.createObject t

    let rec allocateDefault (obj : obj) =
        match obj with
        | null
        | :? referenceRepr -> null
        | :? structureRepr as repr when repr.typ >= 0 ->
            // Case for structs or classes of .NET type
            let t = sourceTypes[repr.typ]
            if t.IsByRefLike then
                internalfailf "Generating test: unable to create byref-like object (type = %O)" t
            if t.ContainsGenericParameters then
                internalfailf "Generating test: unable to create object with generic type parameters (type = %O)" t
            else
                let createdObject = System.Runtime.Serialization.FormatterServices.GetUninitializedObject(t)
                if createMethodSequences
                then
                    match repr.methodSequenceRef with
                    | :? referenceRepr as ref ->
                        methodSequenceObjectIndices[createdObject] <- ref.index
                    | _ -> ()
                createdObject
        | :? structureRepr as repr ->
            // Case for mocked structs or classes
            createMockObject allocateDefault repr.typ
        | :? arrayRepr as repr ->
            let t = sourceTypes[repr.typ]
            let elementType = t.GetElementType()
            if repr.lowerBounds = null then Array.CreateInstance(elementType, repr.lengths) :> obj
            else Array.CreateInstance(elementType, repr.lengths, repr.lowerBounds) :> obj
        | :? stringRepr as repr -> repr.Decode()
        | _ -> obj

    let nullSourceIndex = -1
    let sourceObjects = List<obj>(repr.objects |> Array.map allocateDefault)
    let objReprs = List<obj>(repr.objects)

    let rec decodeValue (createMethodSequenceRefs : bool) (obj : obj) =
        match obj with
        | :? referenceRepr as repr ->
            let sourceObj = sourceObjects[repr.index]
            let index = ref 0
            if createMethodSequenceRefs && methodSequenceObjectIndices.TryGetValue(sourceObj, index)
            then
                let ref = { index = index.Value } : InvokableMethodSequenceRef
                ref : obj
            else sourceObj
        | :? pointerRepr as repr when repr.sightType = intPtrIndex ->
            let shift = decodeValue createMethodSequenceRefs repr.shift :?> int64
            IntPtr(shift) :> obj
        | :? pointerRepr as repr when repr.sightType = uintPtrIndex ->
            let shift = decodeValue createMethodSequenceRefs repr.shift :?> int64 |> uint64
            UIntPtr(shift) :> obj
        | :? pointerRepr as repr ->
            let obj = sourceObjects[repr.index]
            let shift = decodeValue createMethodSequenceRefs repr.shift :?> int64 |> nativeint
            let sightType = sourceTypes[repr.sightType]
            let refWithOffset = System.Runtime.CompilerServices.Unsafe.AddByteOffset(ref obj, shift)
            let pointer = System.Runtime.CompilerServices.Unsafe.AsPointer(ref refWithOffset)
            Pointer.Box(pointer, sightType.MakePointerType())
        | :? structureRepr as repr when repr.typ >= 0 ->
            // Case for structs or classes of .NET type
            if createMethodSequenceRefs && repr.methodSequenceRef <> null
            then
                match repr.methodSequenceRef with
                | :? referenceRepr as ref ->
                    { index = ref.index } : InvokableMethodSequenceRef
                | _ -> internalfail "Unexpected type of method sequence object reference"
            else
                let t = sourceTypes[repr.typ]
                if not t.IsValueType then
                    internalfailf $"Expected value type inside object, but got representation of %s{t.FullName}!"
                let obj = allocateDefault repr
                decodeStructure repr obj
                obj
        | :? structureRepr as repr ->
            // Case for mocked structs or classes
            let obj = createMockObject (decodeValue false) repr.typ
            decodeMockedStructure repr obj
            obj
        | :? arrayRepr -> internalfail "Unexpected array representation inside object!"
        | :? enumRepr as repr ->
            let t = sourceTypes[repr.typ]
            Enum.ToObject(t, repr.underlyingValue)
        | :? stringRepr as str -> str.Decode()
        | _ -> obj

    and decodeFields (fieldsRepr : obj array) obj t : unit =
        let fields = Reflection.fieldsOf false t
        assert(Array.length fields = Array.length fieldsRepr)
        let decodeField (_, field : FieldInfo) repr =
            let value = decodeValue false repr
            field.SetValue(obj, value)
        Array.iter2 decodeField fields fieldsRepr

    and decodeStructure (repr : structureRepr) obj : unit =
        let t = obj.GetType()
        decodeFields repr.fields obj t

    and decodeMockedStructure (repr : structureRepr) obj : unit =
        let fieldsRepr = repr.fields
        if Array.isEmpty fieldsRepr |> not then
            let t = obj.GetType().BaseType
            decodeFields repr.fields obj t

    and decodeArray (repr : arrayRepr) (obj : obj) : unit =
        assert(repr.lowerBounds = null || repr.lengths.Length = repr.lowerBounds.Length)
        let arr = obj :?> Array
        match repr with
        | _ when repr.indices = null ->
            assert(arr.Length = repr.values.Length)
            match repr.lengths, repr.lowerBounds with
            | [|len|], null ->
                let arr = obj :?> Array
                assert(arr.Length = len)
                repr.values |> Array.iteri (fun i r -> arr.SetValue(decodeValue false r, i))
            | lens, lbs ->
                repr.values |> Array.iteri (fun i r ->
                    let value = decodeValue false r
                    let indices = Array.delinearizeArrayIndex i lens lbs
                    arr.SetValue(value, indices))
        | _ ->
            let defaultValue = decodeValue false repr.defaultValue
            let values = Array.map (decodeValue false) repr.values
            Array.fill arr defaultValue
            Array.iter2 (fun (i : int[]) v -> arr.SetValue(v, i)) repr.indices values
            if createCompactRepr then
                let compactRepr = {array = arr; defaultValue = defaultValue; indices = repr.indices; values = values}
                compactRepresentations.Add(arr, compactRepr)

    and decodeObject (repr : obj) (obj : obj) =
        if obj :? ValueType then
            boxedLocations.Add {object = obj} |> ignore
        match repr with
        | :? structureRepr as repr when repr.typ >= 0 ->
            // Case for structs or classes of .NET type
            decodeStructure repr obj
        | :? structureRepr as repr ->
            // Case for mocked structs or classes
            let mockType, t = mockStorage[repr.typ]
            let mockInstanceType =
                match obj with
                | :? Delegate as d -> d.Method.DeclaringType
                | _ -> obj.GetType()
            assert(t = mockInstanceType)
            mockType.Update (decodeValue false) mockInstanceType
            decodeMockedStructure repr obj
        | :? arrayRepr as repr ->
            decodeArray repr obj
        | :? stringRepr -> ()
        | :? ValueType -> ()
        | _ -> internalfail $"decodeObject: unexpected object {obj}"

    let decodeMethodSequence (sequenceRepr : methodSequenceRepr) =
        let decodeArgument (argRepr : obj) =
            match argRepr with
            | :? referenceRepr as repr when repr.index < 0 ->
                invokableMethodSequenceArgument.Variable { index = repr.index }
            | o -> invokableMethodSequenceArgument.Object(decodeValue false o)

        let decodeMethodSequenceElement (elementRepr : obj) =
            match elementRepr with
            | :? methodSequenceCallRepr as call ->
                let method = call.methodRepr.Decode()
                let resultRef =
                    match call.resultObj with
                    | :? referenceRepr as repr -> Some({ index = repr.index } : InvokableMethodSequenceRef)
                    | null -> None
                    | _ -> internalfail "Unexpected result object of call"
                let this =
                    match call.thisArg with
                    | null -> None
                    | thisArg -> Some(decodeArgument thisArg)
                let args = call.args |> Array.map decodeArgument |> Array.toList
                invokableMethodSequenceElement.Call(method, resultRef, this, args)
            | :? methodSequenceStructCtorRepr as createStruct ->
                let typ = sourceTypes[createStruct.typ]
                let resultRef = { index = createStruct.resultObj.index } : InvokableMethodSequenceRef
                invokableMethodSequenceElement.CreateDefaultStruct(typ, resultRef)
            | _ -> internalfail "Unexpected method sequence element repr"

        let decoded = sequenceRepr.elements |> Array.map decodeMethodSequenceElement |> Array.toList |> InvokableMethodSequence
        invokableMethodSequences.Add decoded
        ()

    let () =
        Seq.iter2 decodeObject objReprs sourceObjects
        if createMethodSequences then
            Seq.iter decodeMethodSequence methodSequenceReprs

    member x.DecodeValue (obj : obj) = decodeValue createMethodSequences obj

    member x.CompactRepresentations() = compactRepresentations

    member x.BoxedLocations() = boxedLocations

    member x.MethodSequences() = invokableMethodSequences

    member private x.IsSerializable (t : Type) =
        // TODO: find out which types can be serialized by XMLSerializer
        (t.IsPrimitive && not t.IsEnum) || t = typeof<string> || (t.IsArray && (x.IsSerializable <| t.GetElementType()))

    member private x.CreateArray (arr : Array) =
        let lowerBounds =
            if arr.Rank = 1 && arr.GetLowerBound 0 = 0 then null
            else Array.init arr.Rank arr.GetLowerBound
        let repr : arrayRepr =
            {
                typ = x.RegisterType (arr.GetType())
                defaultValue = null
                indices = null
                values = Array.empty
                lengths = Array.init arr.Rank arr.GetLength
                lowerBounds = lowerBounds
            }
        repr :> obj

    member private x.InitArray (arr : Array) index =
        let contents =
            seq {
                for elem in arr do
                    yield x.Encode elem
            } |> Array.ofSeq
        let encoded = objReprs[index]
        assert(encoded :? arrayRepr)
        let encodedArray = encoded :?> arrayRepr
        objReprs[index] <- {encodedArray with values = contents}

    member private x.RegisterType (typ : Type) =
        match sourceTypes |> Seq.tryFindIndex ((=) typ) with
        | Some idx -> idx
        | None ->
            sourceTypes.Add(typ)
            sourceTypes.Count - 1

    member private x.CreateStructure (obj : obj) =
        let t = obj.GetType()
        let repr : structureRepr = {typ = x.RegisterType t; fields = Array.empty; methodSequenceRef = null}
        repr :> obj

    member private x.InitStructure (repr : structureRepr) (obj : obj) =
        let fields =
            obj.GetType()
            |> Reflection.fieldsOf false
            |> Seq.map (fun (_, field) -> field.GetValue(obj) |> x.Encode)
            |> Array.ofSeq
        { repr with fields = fields }

    member private x.InitClass (obj : obj) index =
        let encoded = objReprs[index]
        assert(encoded :? structureRepr)
        let encodedStructure = encoded :?> structureRepr
        let initialized = x.InitStructure encodedStructure obj
        objReprs[index] <- initialized

    member private x.EncodeStructure (obj : obj) =
        let repr = x.CreateStructure obj :?> structureRepr
        x.InitStructure repr obj

    member x.RepresentEnum (obj : obj) =
        let t = obj.GetType()
        let repr : enumRepr = {typ = x.RegisterType t; underlyingValue = Convert.ChangeType(obj, Enum.GetUnderlyingType t)}
        repr :> obj

    member private x.Bind (obj : obj) (repr : obj) =
        sourceObjects.Add obj
        objReprs.Add repr
        assert(sourceObjects.Count = objReprs.Count)
        sourceObjects.Count - 1

    member x.Encode (obj : obj) : obj =
        match obj with
        | null
        | :? referenceRepr
        | :? structureRepr
        | :? arrayRepr
        | :? pointerRepr
        | :? enumRepr
        | :? stringRepr -> obj
        | _ ->
            let t = obj.GetType()
            if x.IsSerializable t then obj
            else
                match t with
                | _ when t.IsValueType ->
                    if t.IsEnum then x.RepresentEnum obj
                    else x.EncodeStructure obj
                | _ ->
                    let idx =
                        match Seq.tryFindIndex (fun obj' -> Object.ReferenceEquals(obj, obj')) sourceObjects with
                        | Some idx -> idx
                        | None ->
                            match obj with
                            | :? Array as arr ->
                                let idx = x.CreateArray arr |> x.Bind obj
                                x.InitArray arr idx
                                idx
                            | _ ->
                                let idx = x.CreateStructure obj |> x.Bind obj
                                x.InitClass obj idx
                                idx
                    let reference : referenceRepr = {index = idx}
                    reference :> obj

    member x.RepresentIntPtr (shift : int64) =
        let repr : pointerRepr = {index = nullSourceIndex; shift = shift; sightType = intPtrIndex}
        repr :> obj

    member x.RepresentUIntPtr (shift : int64) =
        let repr : pointerRepr = {index = nullSourceIndex; shift = shift; sightType = uintPtrIndex}
        repr :> obj

    member x.RepresentPtr (index : int) (sightType : Type) (shift : int64) =
        let repr : pointerRepr = {index = index; shift = shift; sightType = x.RegisterType sightType}
        repr :> obj

    member x.RepresentStruct (typ : Type) (fields : obj array) (methodSequenceRef : obj) =
        let repr : structureRepr = {typ = x.RegisterType typ; fields = fields; methodSequenceRef = methodSequenceRef }
        repr :> obj

    member x.RepresentMockedStruct (typ : Mocking.Type) (fields : obj array) =
        let repr : structureRepr = {typ = mockStorage.RegisterMockedType typ; fields = fields; methodSequenceRef = null}
        repr :> obj

    member x.AddString index (str : string) =
        objReprs[index] <- stringRepr.Encode str
        { index = index } :> obj

    // Must be used only for decoding of exception message
    member x.DecodeString (repr : obj) : string =
        match repr with
        | :? referenceRepr as repr ->
            let stringRepr = objReprs[repr.index]
            assert(stringRepr :? stringRepr)
            (stringRepr :?> stringRepr).Decode()
        | :? stringRepr as repr ->
            repr.Decode()
        | _ -> internalfail $"DecodeString: unexpected representation {repr}"

    member x.ReserveRepresentation() = x.Bind null null

    member x.AddClass (typ : Type) (fields : obj array) index (methodSequenceRef : obj) =
        let repr : structureRepr = {typ = x.RegisterType typ; fields = fields; methodSequenceRef = methodSequenceRef}
        objReprs[index] <- repr
        { index = index }

    member x.AddMockedClass (typ : Mocking.Type) (fields : obj array) index =
        let repr : structureRepr = {typ = mockStorage.RegisterMockedType typ; fields = fields; methodSequenceRef = null}
        objReprs[index] <- repr
        { index = index }

    member x.AddArray (typ : Type) (contents : obj array) (lengths : int array) (lowerBounds : int array) index =
        let repr : arrayRepr = {typ = x.RegisterType typ; defaultValue = null; indices = null; values = contents; lengths = lengths; lowerBounds = lowerBounds}
        objReprs[index] <- repr
        { index = index }

    member x.AddCompactArrayRepresentation (typ : Type) (defaultValue : obj) (indices : int array array) (values : obj array) (lengths : int array) (lowerBounds : int array) index =
        let repr : arrayRepr = {typ = x.RegisterType typ; defaultValue = defaultValue; indices = indices; values = values; lengths = lengths; lowerBounds = lowerBounds}
        objReprs[index] <- repr
        { index = index }

    member x.AddBoxed (content : obj) index =
        objReprs[index] <- content
        { index = index }

    member x.AddMethodSequenceCall (method : IMethod) (this : obj) (args : obj array) (hasNonVoidResult : bool) =
        let resultObj : obj =
            if hasNonVoidResult
            then
                methodSequenceObjectCounter <- methodSequenceObjectCounter - 1
                { index = methodSequenceObjectCounter }
            else null
        {
            methodRepr = methodRepr.Encode method
            resultObj = resultObj
            thisArg = this
            args = args
        }

    member x.AddMethodSequenceStructCtor (typ : Type) =
        methodSequenceObjectCounter <- methodSequenceObjectCounter - 1
        let resultObj = { index = methodSequenceObjectCounter }
        { typ = x.RegisterType typ; resultObj = resultObj }

    member x.AddMethodSequence (elements : obj array) =
        methodSequenceReprs.Add { elements = elements }

    member x.Serialize (target : memoryRepr) =
        let t = typeof<memoryRepr>
        let p = t.GetProperty("objects")
        p.SetValue(target, objReprs.ToArray())
        let p = t.GetProperty("types")
        p.SetValue(target, sourceTypes |> Seq.map typeRepr.Encode |> Array.ofSeq)

    member x.SerializeMethodSequences() = Seq.toArray methodSequenceReprs
