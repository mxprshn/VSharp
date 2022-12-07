namespace VSharp.Interpreter.IL

open System
open System.Collections.Generic
open VSharp.Interpreter.IL
open VSharp

type PreviousBasicBlockInitPointSearcher() =

    let queue = Queue<codeLocation * codeLocation>()

    let addTarget (loc : codeLocation) =
        let bbOffset = loc.method.CFG.ResolveBasicBlock loc.offset
        if bbOffset = 0<offsets> && loc.offset <> 0<offsets> then
            let initCodeLocation = { loc with offset = 0<offsets> }
            Console.WriteLine $"Created init point: {initCodeLocation} -> {loc}"
            queue.Enqueue(initCodeLocation, loc)
        elif bbOffset <> 0<offsets> then
            // TODO: Rewrite with new incoming CFG API
            let previousBlocks =
                loc.method.CFG.Edges
                |> Seq.where (fun kv -> kv.Value.Contains bbOffset)
                |> Seq.map (fun kv -> kv.Key)
            for blockOffset in previousBlocks do
                let initCodeLocation = { loc with offset = blockOffset }
                Console.WriteLine $"Created init point: {initCodeLocation} -> {loc}"
                queue.Enqueue(initCodeLocation, loc)

    let pick() = if queue.Count = 0 then Seq.empty else queue.Dequeue() |> Seq.singleton

    interface IInitPointSearcher with
        override x.Pick() = pick()
        override x.AddTarget(loc : codeLocation) = addTarget loc
        override x.RemoveTarget(loc : codeLocation) = ()
