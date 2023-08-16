namespace VSharp

open System
open System.Collections.Generic
open System.Reflection
open System.Runtime.InteropServices

[<CustomEquality; CustomComparison>]
type methodDescriptor = {
    methodHandle : nativeint
    declaringTypeVarHandles : nativeint array
    methodVarHandles : nativeint array
    typeHandle : nativeint
}
with
    override x.GetHashCode() =
        HashCode.Combine(x.methodHandle, hash x.declaringTypeVarHandles, hash x.methodVarHandles, x.typeHandle)

    override x.Equals(another) =
        match another with
        | :? methodDescriptor as d -> (x :> IComparable).CompareTo d = 0
        | _ -> false

    interface IComparable with
        override x.CompareTo y =
            match y with
            | :? methodDescriptor as y ->
                compare
                    (x.methodHandle, x.declaringTypeVarHandles, x.methodVarHandles, x.typeHandle)
                    (y.methodHandle, y.declaringTypeVarHandles, y.methodVarHandles, y.typeHandle)
            | _ -> -1

module public Reflection =

    // ----------------------------- Binding Flags ------------------------------

    let staticBindingFlags =
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public
    let instanceBindingFlags =
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public
    let allBindingFlags =
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        staticBindingFlags ||| instanceBindingFlags

    // ------------------------------- Assemblies -------------------------------

    let loadAssembly (assemblyName : string) =
        let assemblies = AssemblyManager.GetAssemblies()
        let dynamicAssemblies = assemblies |> Seq.filter (fun a -> a.IsDynamic)
        let dynamicOption = dynamicAssemblies |> Seq.tryFind (fun a -> a.FullName.Contains(assemblyName))
        match dynamicOption with
        | Some a -> a
        | None -> AssemblyManager.LoadFromAssemblyName assemblyName

    let mscorlibAssembly = typeof<int>.Assembly

    // --------------------------- Metadata Resolving ---------------------------

    let resolveModule (assemblyName : string) (moduleName : string) =
        let assembly =
            match AssemblyManager.GetAssemblies() |> Seq.tryFindBack (fun assembly -> assembly.FullName = assemblyName) with
            | Some assembly -> assembly
            | None ->
                try
                    AssemblyManager.LoadFromAssemblyName assemblyName
                with _ ->
                    AssemblyManager.LoadFromAssemblyPath moduleName
        assembly.Modules |> Seq.find (fun m -> m.FullyQualifiedName = moduleName)

    let resolveMethodBase (assemblyName : string) (moduleName : string) (token : int32) =
        let m = resolveModule assemblyName moduleName
        m.ResolveMethod(token)

    let resolveMethodBaseFromAssembly (assembly: Assembly) (moduleName: string) (token: int32) =
        let m =
            assembly.Modules
            |> Seq.find (fun m -> m.FullyQualifiedName = moduleName)
        m.ResolveMethod(token)

    let private retrieveMethodsGenerics (method : MethodBase) =
        match method with
        | :? MethodInfo as mi -> mi.GetGenericArguments()
        | :? ConstructorInfo -> null
        | _ -> __notImplemented__()

    let resolveModuleFromAssembly (assembly : Assembly) (moduleName : string) =
        assembly.GetModule moduleName

    let resolveTypeFromModule (m : Module) typeToken =
        m.ResolveType(typeToken, null, null)

    let resolveField (method : MethodBase) fieldToken =
        let methodsGenerics = retrieveMethodsGenerics method
        let typGenerics = method.DeclaringType.GetGenericArguments()
        method.Module.ResolveField(fieldToken, typGenerics, methodsGenerics)

    let resolveType (method : MethodBase) typeToken =
        let typGenerics = method.DeclaringType.GetGenericArguments()
        let methodGenerics = retrieveMethodsGenerics method
        method.Module.ResolveType(typeToken, typGenerics, methodGenerics)

    let resolveMethod (method : MethodBase) methodToken =
        let typGenerics = method.DeclaringType.GetGenericArguments()
        let methodGenerics = retrieveMethodsGenerics method
        method.Module.ResolveMethod(methodToken, typGenerics, methodGenerics)

    let resolveToken (method : MethodBase) token =
        let typGenerics = method.DeclaringType.GetGenericArguments()
        let methodGenerics = retrieveMethodsGenerics method
        method.Module.ResolveMember(token, typGenerics, methodGenerics)

    // --------------------------------- Methods --------------------------------

    // TODO: what if return type is generic?
    let getMethodReturnType : MethodBase -> Type = function
        | :? ConstructorInfo -> typeof<Void>
        | :? MethodInfo as m -> m.ReturnType
        | _ -> internalfail "unknown MethodBase"

    let hasNonVoidResult m = (getMethodReturnType m).FullName <> typeof<Void>.FullName

    let hasThis (m : MethodBase) = m.CallingConvention.HasFlag(CallingConventions.HasThis)

    let getFullTypeName (typ : Type) = typ.ToString()

    let getFullMethodName (methodBase : MethodBase) =
        let returnType = getMethodReturnType methodBase |> getFullTypeName
        let declaringType = getFullTypeName methodBase.DeclaringType
        let parameters =
            methodBase.GetParameters()
            |> Seq.map (fun param -> getFullTypeName param.ParameterType)
            |> if methodBase.IsStatic then id else Seq.cons "this"
            |> join ", "
//        let typeParams =
//            if not methodBase.IsGenericMethod then ""
//            else methodBase.GetGenericArguments() |> Seq.map getFullTypeName |> join ", " |> sprintf "[%s]"
        sprintf "%s %s.%s(%s)" returnType declaringType methodBase.Name parameters

    let isArrayConstructor (methodBase : MethodBase) =
        methodBase.IsConstructor && methodBase.DeclaringType.IsArray

    let isDelegateConstructor (methodBase : MethodBase) =
        methodBase.IsConstructor && TypeUtils.isSubtypeOrEqual methodBase.DeclaringType typedefof<Delegate>

    let isDelegate (methodBase : MethodBase) =
        TypeUtils.isSubtypeOrEqual methodBase.DeclaringType typedefof<Delegate> && methodBase.Name = "Invoke"

    let isGenericOrDeclaredInGenericType (methodBase : MethodBase) =
        methodBase.IsGenericMethod || methodBase.DeclaringType.IsGenericType

    let isStaticConstructor (m : MethodBase) =
        m.IsStatic && m.Name = ".cctor"

    let isExternalMethod (methodBase : MethodBase) =
        methodBase.Attributes.HasFlag(MethodAttributes.PinvokeImpl)

    let getAllMethods (t : Type) = t.GetMethods(allBindingFlags)

    let getMethodDescriptor (m : MethodBase) =
        let reflectedType = m.ReflectedType
        let declaringTypeVars =
            if reflectedType.IsGenericType then reflectedType.GetGenericArguments() |> Array.map (fun t -> t.TypeHandle.Value)
            else [||]
        let methodVars =
            if m.IsGenericMethod then m.GetGenericArguments() |> Array.map (fun t -> t.TypeHandle.Value)
            else [||]
        { methodHandle = m.MethodHandle.Value
          declaringTypeVarHandles = declaringTypeVars
          methodVarHandles = methodVars
          typeHandle = reflectedType.TypeHandle.Value }

    let compareMethods (m1 : MethodBase) (m2 : MethodBase) =
        compare (getMethodDescriptor m1) (getMethodDescriptor m2)

    let getArrayMethods (arrayType : Type) =
        let methodsFromHelper = Type.GetType("System.SZArrayHelper") |> getAllMethods
        let makeSuitable (m : MethodInfo) =
            if m.IsGenericMethod then m.MakeGenericMethod(arrayType.GetElementType()) else m
        let concreteMethods = Array.map makeSuitable methodsFromHelper
        Array.concat [concreteMethods; getAllMethods typeof<Array>; getAllMethods arrayType]

    let private isOverrideWithCovarianceReturnType (sourceMethod : MethodInfo) (method : MethodInfo) =
        // Return type covariance case
        Attribute.IsDefined(method, typeof<System.Runtime.CompilerServices.PreserveBaseOverridesAttribute>) &&
        method.Name = sourceMethod.Name &&
            let sourceSig = sourceMethod.GetParameters()
            let targetSig = method.GetParameters()
            targetSig.Length = sourceSig.Length &&
            Array.forall2 (fun (p : ParameterInfo) (p' : ParameterInfo) -> p.ParameterType = p'.ParameterType) sourceSig targetSig

    let isOverrideOf (sourceMethod : MethodInfo) (method : MethodInfo) =
        sourceMethod.GetBaseDefinition() = method.GetBaseDefinition()
        || isOverrideWithCovarianceReturnType sourceMethod method

    let private createSignature (m : MethodInfo) =
        let onlyLastName =
            match m.Name.LastIndexOf('.') with
            | i when i < 0 -> m.Name
            | i -> m.Name.Substring(i + 1)
        if m.IsHideBySig then
            let args =
                m.GetParameters()
                |> Seq.map (fun p -> getFullTypeName p.ParameterType)
                |> join ","
            let genericArgs =
                // If type is 'System.SZArrayHelper' then generic arguments should not be added, because
                // 'SZArrayHelper' implements methods for all vector arrays in .NET and they don't have generics
                if m.IsGenericMethod && m.DeclaringType <> TypeUtils.szArrayHelper.Value then
                    m.GetGenericArguments()
                    |> Seq.map getFullTypeName
                    |> join ","
                else String.Empty
            let returnType = getMethodReturnType m |> toString
            returnType + onlyLastName + genericArgs + args
        else onlyLastName

    let resolveInterfaceOverride (targetType : Type) (interfaceMethod : MethodInfo) =
        let interfaceType = interfaceMethod.DeclaringType
        assert interfaceType.IsInterface
        if interfaceType = targetType then interfaceMethod
        else
            let sign = createSignature interfaceMethod
            let hasTargetSignature (mi : MethodInfo) = createSignature mi = sign
            match targetType with
            | _ when targetType.IsArray -> getArrayMethods targetType |> Seq.find hasTargetSignature
            | _ when targetType.IsInterface -> getAllMethods targetType |> Seq.find hasTargetSignature
            | _ ->
                let interfaceMap = targetType.GetInterfaceMap(interfaceType)
                let targetMethodIndex = Array.findIndex hasTargetSignature interfaceMap.InterfaceMethods
                interfaceMap.TargetMethods[targetMethodIndex]

    let private virtualBindingFlags =
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance |||
            BindingFlags.InvokeMethod ||| BindingFlags.DeclaredOnly

    let isNewSlot (m : MethodInfo) =
        m.Attributes.HasFlag(MethodAttributes.NewSlot)

    let blocksOverride virtualMethod (m : MethodInfo) =
        m.IsFinal || not m.IsVirtual
        || isNewSlot m && not (isOverrideWithCovarianceReturnType virtualMethod m)

    // Finds last type that can override 'virtualMethod' starting from 'targetType'
    // If it's free to override in derived classes of 'targetType', result will be 'targetType'
    // If some type 't' in the hierarchy defines same method or adds new slot for it, result will be base type of 't'
    // If in some type 't' in the hierarchy this method marked 'final', result will be 't'
    let lastCanOverrideType (targetType: Type) (virtualMethod : MethodInfo) =
        match virtualMethod.DeclaringType with
        | t when not virtualMethod.IsVirtual -> t
        | i when i.IsInterface && TypeUtils.typeImplementsInterface targetType i -> targetType
        | i when i.IsInterface -> i
        | t when targetType.IsAssignableTo(t) |> not -> t
        | declaringType ->
            let createHierarchy (t : Type) =
                // TODO: care about generics (GetGenericTypeDefinition) ?
                if t <> declaringType && t <> null then Some (t, t.BaseType)
                else None
            let hierarchy = List.unfold createHierarchy targetType
            let sign = createSignature virtualMethod
            let mutable newSlot = false
            let cancelsOverride (t : Type) =
                let matchedMethods =
                    t.GetMethods(virtualBindingFlags)
                    |> Array.filter (fun m -> createSignature m = sign)
                let canNotOverride (m : MethodInfo) =
                    let blocks = blocksOverride virtualMethod m
                    if blocks && (isNewSlot m || not m.IsVirtual) then
                        newSlot <- true
                    blocks
                Array.forall canNotOverride matchedMethods
            match List.tryFindBack cancelsOverride hierarchy with
            | Some t when newSlot -> t.BaseType
            | Some t -> t
            | None -> targetType

    let canOverrideMethod targetType (virtualMethod : MethodInfo) =
        lastCanOverrideType targetType virtualMethod = targetType

    // TODO: unify with 'lastOverrideType'
    let resolveOverridingMethod targetType (virtualMethod : MethodInfo) =
        assert virtualMethod.IsVirtual
        match virtualMethod.DeclaringType with
        | i when i.IsInterface -> resolveInterfaceOverride targetType virtualMethod
        | _ ->
            let rec resolve targetType =
                assert(targetType <> null)
                if targetType = virtualMethod.DeclaringType then virtualMethod
                else
                    let declaredMethods = targetType.GetMethods(virtualBindingFlags)
                    let resolvedMethod = declaredMethods |> Seq.tryFind (isOverrideOf virtualMethod)
                    match resolvedMethod with
                    | Some resolvedMethod -> resolvedMethod
                    | None when targetType.BaseType = null -> virtualMethod
                    | None -> resolve targetType.BaseType
            resolve targetType

    let typeImplementsMethod targetType (virtualMethod : MethodInfo) =
        let method = resolveOverridingMethod targetType virtualMethod
        not targetType.IsAbstract && method.ReflectedType = targetType

    let hasByRefLikes (method : MethodInfo) =
        method.DeclaringType <> null && method.DeclaringType.IsByRefLike ||
            method.GetParameters() |> Seq.exists (fun pi -> pi.ParameterType.IsByRefLike) ||
            method.ReturnType.IsByRefLike;

    // ----------------------------------- Creating objects ----------------------------------

    let createObject (t : Type) =
        assert(not t.IsByRefLike)
        match t with
        | _ when t = typeof<String> -> String.Empty :> obj
        | _ when TypeUtils.isNullable t -> null
        | _ when t.IsArray -> Array.CreateInstance(typeof<obj>, 1)
        | _ when t.ContainsGenericParameters -> internalfail $"Creating object of open generic type {t}"
        | _ -> System.Runtime.Serialization.FormatterServices.GetUninitializedObject t

    let defaultOf (t : Type) =
        assert(not t.IsByRefLike)
        if t.IsValueType && Nullable.GetUnderlyingType(t) = null && not t.ContainsGenericParameters
            then Activator.CreateInstance t
            else null

    // --------------------------------- Substitute generics ---------------------------------

    let private substituteMethod methodType (m : MethodBase) getMethods =
        let method = getMethods methodType |> Array.tryFind (fun (x : #MethodBase) -> x.MetadataToken = m.MetadataToken)
        match method with
        | Some x -> x
        | None -> internalfailf "unable to find method %s token" m.Name

    let private substituteMethodInfo methodType (mi : MethodInfo) groundK genericK =
        let getMethods (t : Type) = getAllMethods t
        let substituteGeneric (mi : MethodInfo) =
            let args = mi.GetGenericArguments()
            let genericMethod = mi.GetGenericMethodDefinition()
            let mi = substituteMethod methodType genericMethod getMethods
            genericK mi args
        if mi.IsGenericMethod then substituteGeneric mi
        else groundK (substituteMethod methodType mi getMethods :> MethodBase)

    let private substituteCtorInfo methodType ci k =
        let getCtor (t : Type) = t.GetConstructors(allBindingFlags)
        k (substituteMethod methodType ci getCtor :> MethodBase)

    let private substituteMethodBase<'a> methodType (m : MethodBase) (groundK : MethodBase -> 'a) genericK =
        match m with
        | _ when not <| isGenericOrDeclaredInGenericType m -> groundK m
        | :? MethodInfo as mi ->
            substituteMethodInfo methodType mi groundK genericK
        | :? ConstructorInfo as ci ->
            substituteCtorInfo methodType ci groundK
        | _ -> __unreachable__()

    // --------------------------------- Generalization ---------------------------------

    let getGenericTypeDefinition (typ : Type) =
        if typ.IsGenericType then
            let args = typ.GetGenericArguments()
            let genericType = typ.GetGenericTypeDefinition()
            let parameters = genericType.GetGenericArguments()
            genericType, args, parameters
        else typ, [||], [||]

    let generalizeMethodBase (methodBase : MethodBase) =
        let genericType, tArgs, tParams = getGenericTypeDefinition methodBase.DeclaringType
        let genericCase m args = m :> MethodBase, args, m.GetGenericArguments()
        let groundCase m = m, [||], [||]
        let genericMethod, mArgs, mParams = substituteMethodBase genericType methodBase groundCase genericCase
        let genericArgs = Array.append mArgs tArgs
        let genericDefs = Array.append mParams tParams
        genericMethod, genericArgs, genericDefs

    let fullGenericMethodName (methodBase : MethodBase) =
        let genericMethod = generalizeMethodBase methodBase |> fst3
        getFullMethodName genericMethod

    // --------------------------------- Concretization ---------------------------------

    let rec concretizeType (subst : Type -> Type) (typ : Type) =
        if typ.IsGenericParameter then subst typ
        elif typ.IsGenericType then
            let args = typ.GetGenericArguments()
            let args' = args |> Array.map (concretizeType subst)
            if args = args' then typ
            else
                typ.GetGenericTypeDefinition().MakeGenericType(args')
        else typ

    let concretizeMethodBase (m : MethodBase) (subst : Type -> Type) =
        let concreteType = concretizeType subst m.DeclaringType
        let substArgsIntoMethod (mi : MethodInfo) args =
            mi.MakeGenericMethod(args |> Array.map subst) :> MethodBase
        substituteMethodBase concreteType m id substArgsIntoMethod

    let concretizeParameter (p : ParameterInfo) (subst : Type -> Type) =
        assert(p.Member :? MethodBase)
        let method = concretizeMethodBase (p.Member :?> MethodBase) subst
        method.GetParameters() |> Array.find (fun pi -> pi.Name = p.Name)

    let concretizeField (f : fieldId) (subst : Type -> Type) =
        let declaringType = concretizeType subst f.declaringType
        {declaringType = declaringType; name = f.name; typ = concretizeType subst f.typ}

    let public methodToString (m : MethodBase) =
        let hasThis = hasThis m
        let returnsSomething = hasNonVoidResult m
        let argsCount = m.GetParameters().Length
        if m.DeclaringType = null then m.Name
        else sprintf "%s %s.%s(%s)" (if returnsSomething then "nonvoid" else "void") m.DeclaringType.Name m.Name (if hasThis then sprintf "%d+1" argsCount else toString argsCount)

    let concretizeTypeParameters (typ : Type) (values : Type[]) =
        if typ.IsGenericType then
            assert(values.Length = typ.GetGenericArguments().Length)
            typ.MakeGenericType(values)
        else
            assert(values.Length = 0)
            typ

    let concretizeMethodParameters (declaringType : Type) (method : MethodBase) (values : Type[]) =
        match method with
        | :? MethodInfo as mi ->
            let method =
                declaringType.GetMethods(allBindingFlags)
                |> Array.find (fun x -> x.MetadataToken = mi.MetadataToken)
            if method.IsGenericMethod then
                assert(values.Length = method.GetGenericArguments().Length)
                method.MakeGenericMethod(values) :> MethodBase
            else
                method :> MethodBase
        | :? ConstructorInfo as ci ->
            assert(values.Length = 0)
            declaringType.GetConstructors() |> Array.find (fun x -> x.MetadataToken = ci.MetadataToken) :> MethodBase
        | _ -> __notImplemented__()

    // --------------------------------- Fields ---------------------------------

    // TODO: add cache: map from wrapped field to unwrapped

    let wrapField (field : FieldInfo) =
        {declaringType = field.DeclaringType; name = field.Name; typ = field.FieldType}

    let getFieldInfo (field : fieldId) =
        let result = field.declaringType.GetField(field.name, allBindingFlags)
        if result <> null then result
        else field.declaringType.GetRuntimeField(field.name)

    let rec private retrieveFields isStatic (t : Type) =
        let flags = if isStatic then staticBindingFlags else instanceBindingFlags
        t.GetFields(flags) |> Array.sortBy (fun field -> field.Name)

    let retrieveNonStaticFields t = retrieveFields false t

    let fieldsOf isStatic (t : Type) =
        let fields = retrieveFields isStatic t
        let extractFieldInfo (field : FieldInfo) = wrapField field, field
        FSharp.Collections.Array.map extractFieldInfo fields

    // Returns pair (valueFieldInfo, hasValueFieldInfo)
    let fieldsOfNullable typ =
        let fs = fieldsOf false typ
        match fs with
        | [|(f1, _); (f2, _)|] when f1.name.Contains("value", StringComparison.OrdinalIgnoreCase) && f2.name.Contains("hasValue", StringComparison.OrdinalIgnoreCase) -> f1, f2
        | [|(f1, _); (f2, _)|] when f1.name.Contains("hasValue", StringComparison.OrdinalIgnoreCase) && f2.name.Contains("value", StringComparison.OrdinalIgnoreCase) -> f2, f1
        | _ -> internalfailf "%O has unexpected fields {%O}! Probably your .NET implementation is not supported :(" (getFullTypeName typ) (fs |> Array.map (fun (f, _) -> f.name) |> join ", ")

    let stringLengthField, stringFirstCharField =
        let fs = fieldsOf false typeof<string>
        match fs with
        | [|(f1, _); (f2, _)|] when f1.name.Contains("length", StringComparison.OrdinalIgnoreCase) && f2.name.Contains("firstChar", StringComparison.OrdinalIgnoreCase) -> f1, f2
        | [|(f1, _); (f2, _)|] when f1.name.Contains("firstChar", StringComparison.OrdinalIgnoreCase) && f2.name.Contains("length", StringComparison.OrdinalIgnoreCase) -> f2, f1
        | _ -> internalfailf "System.String has unexpected fields {%O}! Probably your .NET implementation is not supported :(" (fs |> Array.map (fun (f, _) -> f.name) |> join ", ")

    let exceptionMessageField =
        {declaringType = typeof<Exception>; name = "_message"; typ = typeof<string>}

    let emptyStringField =
        let fs = fieldsOf true typeof<string>
        match fs |> Array.tryFind (fun (f, _) -> f.name.Contains("empty", StringComparison.OrdinalIgnoreCase)) with
        | Some(f, _) -> f
        | None -> internalfailf "System.String has unexpected static fields {%O}! Probably your .NET implementation is not supported :(" (fs |> Array.map (fun (f, _) -> f.name) |> join ", ")

    let private reinterpretValueTypeAsByteArray (value : obj) size =
        let rawData = Array.create size Byte.MinValue
        let handle = GCHandle.Alloc(rawData, GCHandleType.Pinned)
        try
            Marshal.StructureToPtr(value, handle.AddrOfPinnedObject(), false)
        finally
            handle.Free()
        rawData

    let byteArrayFromField (fieldInfo : FieldInfo) =
        let fieldValue : obj = fieldInfo.GetValue null
        let size = TypeUtils.internalSizeOf fieldInfo.FieldType
        reinterpretValueTypeAsByteArray fieldValue size

    // ------------------------------ Layout Utils ------------------------------

    let getFieldOffset field =
        if wrapField field = stringFirstCharField then 0
        else CSharpUtils.LayoutUtils.GetFieldOffset field

    let getFieldIdOffset fieldId =
        if fieldId = stringFirstCharField then 0
        else getFieldInfo fieldId |> CSharpUtils.LayoutUtils.GetFieldOffset

    let blockSize (t : Type) =
        if t.IsValueType then TypeUtils.internalSizeOf t
        else CSharpUtils.LayoutUtils.ClassSize t

    let arrayElementsOffset = CSharpUtils.LayoutUtils.ArrayElementsOffset

    let stringElementsOffset = CSharpUtils.LayoutUtils.StringElementsOffset

    let fieldIntersects (fieldId : fieldId) =
        let fieldInfo = getFieldInfo fieldId
        let fieldType = fieldInfo.FieldType
        if fieldType.ContainsGenericParameters then false
        else
            let offset = getFieldIdOffset fieldId
            let size = TypeUtils.internalSizeOf fieldType
            let intersects o s = o + s > offset && o < offset + size
            let fields = fieldsOf false fieldId.declaringType
            let checkIntersects (_, fieldInfo : FieldInfo) =
                let o = CSharpUtils.LayoutUtils.GetFieldOffset fieldInfo
                let s = TypeUtils.internalSizeOf fieldInfo.FieldType
                intersects o s
            let intersectingFields = Array.filter checkIntersects fields
            Array.length intersectingFields > 1

    // -------------------------------- Types --------------------------------

    let private cachedTypes = Dictionary<Type, bool>()

    let rec private isReferenceOrContainsReferencesHelper (t : Type) =
        if t.IsValueType |> not then true
        else
            t.GetFields(allBindingFlags)
            |> Array.exists (fun field -> field.FieldType = t || isReferenceOrContainsReferencesHelper field.FieldType)

    let isReferenceOrContainsReferences (t : Type) =
        let result = ref false
        if cachedTypes.TryGetValue(t, result) then result.Value
        else
            let result = isReferenceOrContainsReferencesHelper t
            cachedTypes.Add(t, result)
            result

    let isBuiltInType (t: Type) =
        let builtInAssembly = mscorlibAssembly
        t.Assembly = builtInAssembly
