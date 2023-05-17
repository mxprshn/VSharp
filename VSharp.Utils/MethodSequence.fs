namespace VSharp.MethodSequences

open System
open System.Collections.Generic
open System.Reflection
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

type InvokableMethodSequenceRef = { index : int }
with
    override x.ToString() = $"ref_{x.index}"

type internal invokableMethodSequenceArgument =
    | Variable of InvokableMethodSequenceRef
    | Object of obj

type internal invokableMethodSequenceElement =
    | Call of MethodBase * InvokableMethodSequenceRef option * invokableMethodSequenceArgument option * invokableMethodSequenceArgument list
    | CreateDefaultStruct of Type * InvokableMethodSequenceRef

    override x.ToString() =
        let separator = ", "
        let receiverString (this : invokableMethodSequenceArgument option) (method : MethodBase) =
            match this with
            | None -> $"{method.DeclaringType.Name}"
            | Some arg -> $"{arg.ToString()}"
        match x with
        | Call(method, Some ref, this, args) ->
            $"{ref} = {receiverString this method}.{method.Name}({join separator (args |> List.map (fun a -> a.ToString()))})"
        | Call(method, None, this, args) ->
            $"{receiverString this method}.{method.Name}({join separator (args |> List.map (fun a -> a.ToString()))})"
        | CreateDefaultStruct(typ, ref) ->
            $"{ref} = {typ.Name}()"

type InvokableMethodSequence internal (elements : invokableMethodSequenceElement list) =

    member x.CreatesRef (ref : InvokableMethodSequenceRef) = false

    // TODO: out parameters
    member x.Invoke() : IDictionary<InvokableMethodSequenceRef, obj> =
        let createdObjects = Dictionary<InvokableMethodSequenceRef, obj>()
        let call (element : invokableMethodSequenceElement) =
            match element with
            | CreateDefaultStruct(typ, ref) ->
                createdObjects[ref] <- Activator.CreateInstance typ
            | Call(method, resultRef, this, args) ->
                let mapArg (arg : invokableMethodSequenceArgument) =
                    match arg with
                    | Variable ref -> createdObjects[ref]
                    | Object o -> o
                let this = bindToObj mapArg this
                let args = args |> List.map mapArg |> List.toArray
                let result =
                    if method.IsConstructor then
                        (method :?> ConstructorInfo).Invoke args
                    else
                        method.Invoke(this, args)
                match resultRef with
                | Some resultRef ->
                    createdObjects[resultRef] <- result
                | None -> ()
        Seq.iter call elements
        createdObjects

    override x.ToString() =
        elements |> Seq.map (fun e -> e.ToString()) |> join ";\n"
