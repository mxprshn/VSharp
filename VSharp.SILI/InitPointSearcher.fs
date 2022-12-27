namespace VSharp.Interpreter.IL

open System
open System.Collections.Generic
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp

type PreviousBasicBlockInitPointSearcher() =

    let queue = Queue<codeLocation * BasicBlock List>()
    
    let hasZeroStackBalance (block : BasicBlock) =
        let instr = block.Method.ILRewriter.InstrFromOffset (int block.StartOffset)        
        match instr.stackState with
        | None -> false
        | Some state -> state.IsEmpty
    
    let getIncomingZeroBalanceBlocks (loc : codeLocation) =
        let found = List<BasicBlock>()
        let toVisit = Queue<BasicBlock>()
        toVisit.Enqueue loc.BasicBlock
        while toVisit.Count > 0 do
            let visited = toVisit.Dequeue()
            if visited <> loc.BasicBlock && hasZeroStackBalance visited then
                found.Add visited
            else
                visited.IncomingCFGEdges |> Seq.iter toVisit.Enqueue
        found

    let addTarget (loc : codeLocation) =
        queue.Enqueue(loc, getIncomingZeroBalanceBlocks loc)

    let pick() =
        if queue.Count = 0 then
            Seq.empty
        else
            let targetLoc, incomingEdges = queue.Dequeue()
            incomingEdges |> Seq.map (fun e ->
                let fromLoc = {method = targetLoc.method; offset = e.StartOffset}
                fromLoc, targetLoc
            )

    interface IInitPointSearcher with
        override x.Pick() = pick()
        override x.AddTarget(loc : codeLocation) = addTarget loc
        override x.RemoveTarget(loc : codeLocation) = ()
