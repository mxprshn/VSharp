namespace VSharp.MethodSequences

open System
open System.Collections.Generic
open System.Reflection
open VSharp

[<CLIMutable>]
[<Serializable>]
type methodSequenceArgumentRepr = {
    index : int32
    concrete : obj
    isDefault : bool
    typ : typeRepr
}

[<CLIMutable>]
[<Serializable>]
type methodSequenceElementRepr = {
    methodToken : int32
    methodType : typeRepr
    methodGenericArgs : typeRepr array
    returnTo : int32
    returnType : typeRepr
    arguments : methodSequenceArgumentRepr array
    isDefaultStructCtor : bool
}

type variableId = { typ : Type; index : int }

type methodSequenceArgument =
    | Default of Type
    | ConcretePrimitive of Type * obj
    | Variable of variableId

    override x.ToString() =
        match x with
        | Default typ -> $"default({typ})"
        | ConcretePrimitive(_, value) -> $"{value}"
        | Variable({ typ = typ; index = index }) -> $"{typ.Name}_{index}"

type methodSequenceElement =
    | Call of IMethod * variableId option * methodSequenceArgument list
    | CreateDefaultStruct of variableId

    override x.ToString() =
        let separator = ", "
        match x with
        | Call(method, Some { typ = typ; index = idx }, args) ->
            $"{typ.Name}_{idx} = {method.DeclaringType.Name}.{method.Name}({join separator (args |> List.map (fun a -> a.ToString()))})"
        | Call(method, None, args) ->
            $"{method.DeclaringType.Name}.{method.Name}({join separator (args |> List.map (fun a -> a.ToString()))})"
        | CreateDefaultStruct { typ = typ; index = index } ->
            $"{typ}_{index} = default({typ})"

type private invokableMethodSequenceElement =
    | Call of MethodBase * variableId option * methodSequenceArgument list
    | CreateDefaultStruct of variableId

[<AllowNullLiteral>]
type InvokableMethodSequence(reprs : methodSequenceElementRepr array) =
    let reprToArgument (repr : methodSequenceArgumentRepr) =
        match repr with
        | { index = -1; concrete = null; isDefault = true; typ = typeRepr } ->
            let typ = Serialization.decodeType typeRepr
            Default(typ)
        | { index = -1; concrete = obj; isDefault = false; typ = typeRepr } ->
            let typ = Serialization.decodeType typeRepr
            let obj =
                if typ.IsByRef && typ.GetElementType().IsEnum then
                    Enum.ToObject(typ.GetElementType(), obj)
                else obj
            ConcretePrimitive(typ, obj)
        | { index = index; concrete = null; isDefault = false; typ = typeRepr } ->
            let typ = Serialization.decodeType typeRepr
            Variable({ typ = typ; index = index })
        | _ -> __unreachable__()

    let reprToElement (repr : methodSequenceElementRepr) =
        let ret =
            if repr.returnTo <> -1 then { typ = Serialization.decodeType repr.returnType; index = repr.returnTo } |> Some
            else None
        match repr with
        | { isDefaultStructCtor = true } ->
            CreateDefaultStruct ret.Value
        | _ ->
            let methodType = Serialization.decodeType repr.methodType
            let methodArgs = Array.map Serialization.decodeType repr.methodGenericArgs
            let methodArgs = if methodArgs.Length = 0 then Type.EmptyTypes else methodArgs
            let typeArgs = if methodType.GenericTypeArguments.Length = 0 then Type.EmptyTypes else methodType.GenericTypeArguments
            let method = methodType.Module.ResolveMethod(repr.methodToken, typeArgs, methodArgs)
            let method = Reflection.concretizeMethodParameters methodType method methodArgs
            let args = Seq.map reprToArgument repr.arguments |> Seq.toList
            Call(method, ret, args)

    let elements = Seq.map reprToElement reprs |> Seq.toList

    member x.Invoke() : obj =
        let locals = Dictionary<variableId, obj>()
        let argToObj (arg : methodSequenceArgument) =
            match arg with
            | Default typ ->
                if typ.IsValueType && Nullable.GetUnderlyingType(typ) = null then
                    Activator.CreateInstance typ
                else null
            | ConcretePrimitive(_, obj) -> obj
            | Variable id ->
                if locals.ContainsKey id then locals[id]
                else null
        let call (element : invokableMethodSequenceElement) =
            match element with
            | Call(method, ret, args) ->
                let outIndices = List<int>()
                let this, argObjsArray, argsArray =
                    if Reflection.hasThis method then
                        let argsArray = List.tail args |> List.toArray
                        let this = List.head args |> argToObj
                        let argObjsArray =
                            seq {
                                let parameters = method.GetParameters()
                                for i in 0..(argsArray.Length - 1) do
                                    if parameters[i].IsOut then
                                        outIndices.Add i
                                    yield argToObj (argsArray[i])
                            } |> Seq.toArray
                        this, argObjsArray, argsArray
                    else
                        let argsArray = args |> List.toArray
                        null, Array.map argToObj argsArray, argsArray
                let result =
                    if method.IsConstructor then
                        (method :?> ConstructorInfo).Invoke(argObjsArray)
                    else
                        method.Invoke(this, argObjsArray)
                if method.IsConstructor then
                    match args.Head with
                    | Variable id ->
                        locals[id] <- result
                    | _ -> __unreachable__()
                for i in outIndices do
                    match argsArray[i] with
                    | Variable id ->
                        locals[id] <- argObjsArray[i]
                    | _ -> __unreachable__()
                match ret with
                | Some ret -> locals[ret] <- result
                | _ -> ()
                result
            | CreateDefaultStruct({ typ = typ } as var) ->
                // TODO: replace with isStruct
                assert(typ.IsValueType && not <| typ.IsPrimitive)
                let createdStruct = Activator.CreateInstance typ
                locals[var] <- createdStruct
                createdStruct
        let rec invokeRec (sequence : invokableMethodSequenceElement list) =
            match sequence with
            | [lastCall] ->
                call lastCall
            | currentCall :: otherCalls ->
                call currentCall |> ignore
                invokeRec otherCalls
            | [] -> __unreachable__()
        invokeRec elements

module MethodSequence =

    let public argumentToRepr (argument : methodSequenceArgument) =
        match argument with
        | Default typ ->
            {
                index = -1
                concrete = null
                isDefault = true
                typ = Serialization.encodeType typ
            }
        | ConcretePrimitive(typ, obj) ->
            {
                index = -1
                concrete = obj
                isDefault = false
                typ = Serialization.encodeType typ
            }
        | Variable({ typ = typ; index = idx }) ->
            {
                index = idx
                concrete = null
                isDefault = false
                typ = Serialization.encodeType typ
            }

    let public elementToRepr (element : methodSequenceElement) =
        match element with
        | methodSequenceElement.Call(method, ret, args) ->
            let retIndex, retType =
                match ret with
                | Some({ typ = typ; index = idx }) ->
                    idx, Serialization.encodeType typ
                | None -> -1, Serialization.nullTypeRepr
            {
                methodToken = method.MetadataToken
                methodType = Serialization.encodeType method.DeclaringType
                methodGenericArgs = Array.map Serialization.encodeType method.GenericArguments
                returnTo = retIndex
                returnType = retType
                arguments = List.map argumentToRepr args |> List.toArray
                isDefaultStructCtor = false
            }
        | methodSequenceElement.CreateDefaultStruct({ typ = typ; index = idx }) ->
            {
                methodToken = -1
                methodType = Serialization.nullTypeRepr
                methodGenericArgs = Array.empty
                returnTo = idx
                returnType = Serialization.encodeType typ
                arguments = Array.empty
                isDefaultStructCtor = true
            }
