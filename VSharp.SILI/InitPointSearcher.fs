namespace VSharp.Searchers

open System
open System.Collections.Generic
open VSharp.Interpreter.IL
open VSharp

type PreviousBasicBlockInitPointSearcher() =

    let queue = Queue<codeLocation * codeLocation>()

    let addTarget (loc : codeLocation) =
        let bbIndex = loc.method.CFG.ResolveBasicBlockIndex loc.offset
        if not <| (bbIndex = 0 && loc.offset = 0<offsets>) then
            let initPointBbIndex = if bbIndex = 0 then bbIndex else bbIndex - 1
            let initOffset = loc.method.CFG.SortedOffsets.[initPointBbIndex]
            let initCodeLocation = { loc with offset = initOffset }
            Console.WriteLine $"Created init point: {initCodeLocation} -> {loc}"
            queue.Enqueue(initCodeLocation, loc)

    let pick() = if queue.Count = 0 then Seq.empty else queue.Dequeue() |> Seq.singleton

    interface IInitPointSearcher with
        override x.Pick() = pick()
        override x.AddTarget(loc : codeLocation) = addTarget loc
        override x.RemoveTarget(loc : codeLocation) = ()
