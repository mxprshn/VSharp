namespace VSharp.Interpreter.IL

open System
open FSharpx.Collections
open Microsoft.FSharp.Collections
open VSharp
open VSharp.Core

type sequenceElement =
    | Call of stackKey option * Method * stackKey list

type methodSequenceState =
    {
        cilState : cilState
        currentSequence : sequenceElement list
        targets : cilState list
        localVariables : PersistentHashMap<Type, int>
    }

module MethodSequenceState =

    let private getNonPrimitiveParameters (method : Method) =
        method.Parameters |> Seq.map (fun pi -> pi.ParameterType) |> Seq.filter (fun t -> not t.IsPrimitive) |> Seq.toList

    let createInitial (targetState : cilState) =
        let state = Memory.EmptyState()
        let nopMethod = Application.getMethod Loader.MethodSequenceUtilsNop
        let cilState = CilStateOperations.makeInitialState nopMethod state
        {
            cilState = cilState
            currentSequence = List.empty
            targets = [targetState]
            localVariables = PersistentHashMap.empty
        }
