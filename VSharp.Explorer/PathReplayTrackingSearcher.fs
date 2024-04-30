namespace VSharp.Explorer

open System
open System.Collections.Generic
open VSharp
open VSharp.Interpreter.IL.CilState

type forkIndices = int list

type internal PathReplayTrackingSearcher(baseSearcher : IForwardSearcher) =
    let baseSearcher = baseSearcher
    
    // TODO: use the same trees here and in ExecutionTreeSearcher
    let trees = Dictionary<Method, ExecutionTree<cilState>>()  
    let methods = List<Method>()

    let init (initialStates : cilState seq) =
        if trees.Count > 0 then
            invalidOp "Trying to init non-empty replay tracking searcher"
        for method, methodStates in initialStates |> Seq.groupBy (_.EntryMethod) do
            methods.Add method
            if Seq.length methodStates > 1 then
                invalidOp "Cannot init replay tracking searcher with more than 1 initial state for method"
            trees[method] <- ExecutionTree(Seq.head methodStates)
        baseSearcher.Init initialStates

    let remove (state : cilState) =
        let entryMethod = state.EntryMethod
        let tree = ref null
        if trees.TryGetValue(entryMethod, tree) then
            let tree = tree.Value
            tree.Remove state |> ignore
            if tree.StatesCount = 0 then
                let wasRemoved = trees.Remove entryMethod
                assert wasRemoved
                let wasRemoved = methods.Remove entryMethod
                assert wasRemoved
        baseSearcher.Remove state
        
    let reset() =
        trees.Clear()
        methods.Clear()
        baseSearcher.Reset()

    let update (parent : cilState) newStates =
        if Seq.length newStates > 2 then
            Console.WriteLine ""
        let entryMethod = parent.EntryMethod
        let tree = ref null
        if trees.TryGetValue(entryMethod, tree) then
            tree.Value.AddFork parent newStates |> ignore
        baseSearcher.Update(parent, newStates)
        
    member this.ExportForkIndices (state : cilState) : forkIndices =
        let entryMethod = state.EntryMethod
        let tree = ref null
        if trees.TryGetValue(entryMethod, tree) then
            tree.Value.GetForkIndices state
        else
            []

    interface IForwardSearcher with
        member this.Init initialStates = init initialStates
        member this.Pick() = baseSearcher.Pick()
        member this.Pick selector = baseSearcher.Pick selector
        member this.Remove state = remove state
        member this.Reset() = reset()
        member this.States() = baseSearcher.States()
        member this.StatesCount = Seq.sumBy (fun (t : ExecutionTree<cilState>) -> t.StatesCount) trees.Values
        member this.Update(parent, newStates) = update parent newStates
