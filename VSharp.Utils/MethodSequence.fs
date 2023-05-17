namespace VSharp.MethodSequences

open System
open VSharp

type variableId = { typ : Type; index : int }

type methodSequenceArgument =
    | Default of Type
    | Variable of variableId
    | Hole of Type

    override x.ToString() =
        match x with
        | Default typ -> $"default<{typ}>"
        | Variable({ typ = typ; index = index }) -> $"{typ.Name}_{index}"
        | Hole typ -> $"hole<{typ}>"

type methodSequenceElement =
    | Call of IMethod * variableId option * methodSequenceArgument option * methodSequenceArgument list
    | CreateDefaultStruct of variableId

    override x.ToString() =
        let separator = ", "
        let receiverString (this : methodSequenceArgument option) (method : IMethod) =
            match this with
            | None -> $"{method.DeclaringType.Name}"
            | Some arg -> $"{arg.ToString()}"
        match x with
        | Call(method, Some { typ = typ; index = idx }, this, args) ->
            $"{typ.Name}_{idx} = {receiverString this method}.{method.Name}({join separator (args |> List.map (fun a -> a.ToString()))})"
        | Call(method, None, this, args) ->
            $"{receiverString this method}.{method.Name}({join separator (args |> List.map (fun a -> a.ToString()))})"
        | CreateDefaultStruct { typ = typ; index = index } ->
            $"{typ}_{index} = {typ.Name}()"

(*type private invokableMethodSequenceElement =
    | Call of MethodBase * variableId option * methodSequenceArgument list
    | CreateDefaultStruct of variableId

[<AllowNullLiteral>]
type InvokableMethodSequence(reprs : methodSequenceElementRepr array) =
    member x.Invoke() : obj =
        let locals = Dictionary<variableId, obj>()
        let argToObj (arg : methodSequenceArgument) =
            match arg with
            | Default typ ->
                if typ.IsValueType && Nullable.GetUnderlyingType(typ) = null then
                    Activator.CreateInstance typ
                else null
            | Variable id ->
                if locals.ContainsKey id then locals[id]
                else null
            | Hole _ -> __unreachable__()
        let call (element : invokableMethodSequenceElement) =
            match element with
            | Call(method, ret, args) ->
                let outIndices = List<int>()
                let this, argObjsArray, argsArray =
                    if Reflection.hasThis method && not method.IsConstructor then
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
                    match ret with
                    | Some id ->
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
        invokeRec elements*)
