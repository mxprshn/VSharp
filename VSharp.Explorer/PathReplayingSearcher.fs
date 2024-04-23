namespace VSharp.Explorer

open System
open System.Collections
open System.Collections.Generic
open VSharp
open VSharp.Interpreter.IL.CilState

type internal PathReplayingSearcher(forkIndices : int list) =

    let currentForkIndices = Queue<int>()
    let mutable currentState : cilState option = None

    do
        List.iter currentForkIndices.Enqueue forkIndices

    let init (initialStates : cilState seq) =
        if Seq.isEmpty initialStates then
            internalfailf "Initial states sequence was empty"
        if Seq.isEmpty initialStates || Seq.tail initialStates |> Seq.isEmpty |> not then
            internalfailf "Replaying searcher works with single initial state only"
        currentState <- Some <| Seq.head initialStates

    let pick() = currentState

    let remove (state : cilState) =
        match currentState with
        | Some currentStateValue when currentStateValue = state ->
            currentState <- None
        | _ -> ()

    let reset() =
        currentState <- None

    let states() =
        match currentState with
        | Some currentStateVal -> Seq.singleton currentStateVal
        | None -> Seq.empty

    let statesCount() =
        match currentState with
        | Some _ -> 1
        | None -> 0

    let update parent newStates =
        // TODO: there is always an extra update which is not tracked by replay. It's because we call update after export,
        // and then remove the updated state. Think how to improve that
        if currentForkIndices.Count > 0 then
            let forkIndex = currentForkIndices.Dequeue()
            if forkIndex = 0 then
                currentState <- Some parent
            else
                let newStates = Seq.toArray newStates
                assert(forkIndex < newStates.Length + 1)
                currentState <- Some newStates[forkIndex - 1]

    interface IForwardSearcher with
        member this.Init initialStates = init initialStates
        member this.Pick() = pick()
        member this.Pick _ = pick()
        member this.Remove state = remove state
        member this.Reset() = reset()
        member this.States() = states()
        member this.StatesCount = statesCount()
        member this.Update(parent, newStates) = update parent newStates
