namespace VSharp.MethodSequences

open System
open System.Collections.Generic
open System.Text
open VSharp
open VSharp.Core.API
open VSharp.Interpreter.IL

type internal methodSequenceState =
    {
        cilState : cilState
        upcomingSequence : methodSequenceElement list
        currentSequence : methodSequenceElement list
        id : uint
    }

    override x.ToString() =
        let mutable sb = StringBuilder($"[{x.id}\n")
        for called in x.currentSequence do
            sb <- sb.AppendLine $"\t{called}"
        sb <- sb.AppendLine "\t ---"
        for toCall in x.upcomingSequence do
            sb <- sb.AppendLine $"\t{toCall}"
        sb <- sb.Append "]"
        sb.ToString()

module internal MethodSequenceHelpers =

    let baseMethod = Application.getMethod Loader.MethodSequenceBase

    let variableIndices = Dictionary<Type, int>()

    let mutable private currentId = 0u

    let isPrimitive (t : Type) = t.IsPrimitive || t.IsEnum

    let rec canBeCreatedBySolver (t : Type) =
        isPrimitive t ||
            t = typeof<string> ||
            (Types.IsNullable t && canBeCreatedBySolver <| Nullable.GetUnderlyingType t) ||
            (t.IsArray && canBeCreatedBySolver <| t.GetElementType()) ||
            (t.IsByRef && canBeCreatedBySolver <| t.GetElementType())

    let isStruct (t : Type) = t.IsValueType && not t.IsPrimitive && not t.IsEnum

    let unwrapRefType (typ : Type) =
        if typ.IsByRef then typ.GetElementType()
        else typ

    let getNextStateId() =
        currentId <- currentId + 1u
        currentId

    let getElementMethod (element : methodSequenceElement) =
        match element with
        | methodSequenceElement.Call(method, _, _, _) -> Some method
        | _ -> None

    let getAllMethods (state : methodSequenceState) =
        seq {
            yield! state.currentSequence |> List.choose getElementMethod
            yield! state.upcomingSequence |> List.choose getElementMethod
        }

    let hasInstanceThis (method : IMethod) = not method.IsConstructor && method.HasThis

    let getThisAndParameterTypes (method : IMethod) =
        seq {
            if hasInstanceThis method then
                yield method.DeclaringType

            yield! method.Parameters |> Array.map (fun pi -> pi.ParameterType)
        }

    let getReturnVar (element : methodSequenceElement) =
        match element with
        | Call(_, var, _, _) -> var
        | CreateDefaultStruct var -> Some var

    let getFreshVariableId (typ : Type) : variableId =
        assert(not typ.IsByRef)
        let index = if variableIndices.ContainsKey typ then variableIndices[typ] else 0
        variableIndices[typ] <- index + 1
        { typ = typ; index = index }

    let createUnknownArgumentOfType (typ : Type) =
        let unwrapped = unwrapRefType typ
        if not <| canBeCreatedBySolver unwrapped then
            Hole unwrapped
        else
            getFreshVariableId unwrapped |> Variable

    let isInMethod (state : methodSequenceState) =
        state.cilState.currentLoc.method <> baseMethod

    let getExistingObjectIds (state : methodSequenceState) =
        // TODO: we should also consider out vars
        state.currentSequence |> List.choose getReturnVar

    let thisAndArguments (this : methodSequenceArgument option) (args : methodSequenceArgument list) =
        seq {
            match this with
            | Some this -> yield this
            | None -> ()
            yield! args
        }
