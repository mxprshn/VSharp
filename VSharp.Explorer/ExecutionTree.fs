namespace VSharp.Explorer

open VSharp
open System.Collections.Generic

type private executionTreeNode<'a> =
    {
        children : List<executionTreeNode<'a>>
        parent : executionTreeNode<'a> option
        mutable depth : int
        states : List<'a>
        index : int
    }

[<AllowNullLiteral>]
type ExecutionTree<'a when 'a : equality>(initialState : 'a) =

    let stateToNode = Dictionary<'a, executionTreeNode<'a>>()
    let initialStateNode =
        {
            children = List<executionTreeNode<'a>>()
            parent = None
            depth = 0
            states = List<'a>()
            index = -1 
        }

    do
        initialStateNode.states.Add initialState
        stateToNode[initialState] <- initialStateNode

    let rec remove node =
        match node.parent with
        | Some parent ->
            let children = parent.children
            let wasRemoved = children.Remove node
            assert wasRemoved
            if children.Count = 0 then
                remove parent
        | _ -> ()

    let rec pick current (randomNonNegIntFunc : unit -> int) =
        let randomInt = randomNonNegIntFunc()
        if randomInt < 0 then
            invalidArg (nameof randomNonNegIntFunc) "Random function returned a negative number"
        if current.children.Count = 0 then
            assert(current.states.Count > 0)
            let stateIndex = randomInt % current.states.Count
            current.states[stateIndex]
        else
            let childIndex = randomInt % current.children.Count
            pick current.children[childIndex] randomNonNegIntFunc
            
    let rec getExecutionPathRec node acc =
        match node with
        | Some node ->
            if node.index = -1 then
                let repeatCount = node.depth
                let repeated = List.replicate repeatCount 0
                repeated @ acc
            else
                let parentDepth = node.parent.Value.depth
                let repeatCount = node.depth - parentDepth - 1
                let repeated = List.replicate repeatCount 0
                getExecutionPathRec node.parent (node.index :: (repeated @ acc)) 
        | None -> __unreachable__()

    member x.States with get() = stateToNode.Keys |> seq

    member x.StatesCount with get() = stateToNode.Count

    member x.AddFork parent children =
        let parentNode = ref initialStateNode
        if not <| stateToNode.TryGetValue(parent, parentNode) then
            false
        else
            let parentNode = parentNode.Value
            if Seq.isEmpty children then
                parentNode.depth <- parentNode.depth + 1
                true
            else
                let newParentNode =
                    {
                        children = List<executionTreeNode<'a>>()
                        parent = Some parentNode
                        depth = parentNode.depth + 1
                        states = List<'a>()
                        index = 0 
                    }
                newParentNode.states.Add parent
                parentNode.children.Add newParentNode
                stateToNode[parent] <- newParentNode
                let wasRemoved = parentNode.states.Remove parent
                assert wasRemoved
                for i, child in Seq.indexed children do
                    let childNode =
                        {
                            children = List<executionTreeNode<'a>>()
                            parent = Some parentNode
                            depth = parentNode.depth + 1
                            states = List<'a>()
                            index = i + 1 // + 1, cause the 0th one is the parent
                        }
                    childNode.states.Add child
                    parentNode.children.Add childNode
                    stateToNode[child] <- childNode
                true

    member x.Remove state =
        let node = ref initialStateNode
        if not <| stateToNode.TryGetValue(state, node) then
            false
        else
            let node = node.Value
            remove node
            let wasRemoved = stateToNode.Remove state
            assert wasRemoved
            true

    member x.RandomPick randomNonNegIntFunc =
        if stateToNode.Count = 0 then
            None
        else
            pick initialStateNode randomNonNegIntFunc |> Some
            
    member x.GetForkIndices state =
        let leafNode = ref initialStateNode
        if not <| stateToNode.TryGetValue(state, leafNode) then
            []
        else
            getExecutionPathRec (Some leafNode.Value) []
            
            
