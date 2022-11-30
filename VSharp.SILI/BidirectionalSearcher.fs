namespace VSharp.Interpreter.IL

open System.Collections.Generic
open FSharpx.Collections
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL

type BidirectionalSearcher(forward : IForwardSearcher, backward : IBackwardSearcher, targeted : ITargetedSearcher) =

//    let starts = Queue<MethodBase>()
//    let addedStarts = HashSet<MethodBase>()
//    let mutable inverseReachability : Dictionary<MethodBase, HashSet<MethodBase>> = null

//    let rememberStart (m : MethodBase) =
//        if addedStarts.Contains(m) then ()
//        else
//            addedStarts.Add(m) |> ignore
//            starts.Enqueue(m)
//    let startFrom (m : MethodBase) =
//        assert(addedStarts.Contains(m))
////        Logger.warning "Starting for method = %s" (Reflection.getFullMethodName m)
//        Start(Instruction(0x00, m))
//    let getInverse (ip : ip) =
//        let m = CilStateOperations.methodOf ip
//        if inverseReachability.ContainsKey(m) then inverseReachability.[m]
//        else HashSet<_>()



    interface IBidirectionalSearcher with
//            let _, inverseReachability' = CFG.buildMethodsReachabilityForAssembly m
//            inverseReachability <- inverseReachability'
//            Seq.iter (fun p -> rememberStart p.loc.method) mainPobs
//        override x.PriorityQueue _ = StackFrontQueue() :> IPriorityQueue<cilState>

        override x.Init cilStates mainPobs =
            backward.Init mainPobs
//            let start : cilState = CilStateOperations.makeInitialState m (ExplorerBase.FormInitialStateWithoutStatics m)
            forward.Init cilStates

        override x.Statuses() = backward.Statuses()
        override x.Answer pob pobStatus = backward.Answer pob pobStatus
        override x.UpdatePobs parent child =
            backward.Update parent child
        override x.UpdateStates parent children =
            if not <| CilStateOperations.isIsolated parent then
                forward.Update (parent, children)
                backward.AddBranch parent |> ignore
                Seq.iter (backward.AddBranch >> ignore) children
            else
                let reached = targeted.Update parent children
                Seq.iter (backward.AddBranch >> ignore) reached
        override x.States() = forward.States()

        override x.Pick () =
            match forward.Pick() with
            | Some s -> GoFront s
            | None -> Stop
//            match backward.Pick() with
//            | Propagate(s,p) -> GoBack (s,p)
//            | InitTarget(from, pobs) ->
//                let tos = Seq.map (fun (pob : pob) -> Instruction(pob.loc.offset, pob.loc.method)) pobs
//                targeted.SetTargets from tos
//                match targeted.Pick() with
//                | Some s -> GoFront s
//                | None -> internalfail "Targeted searcher must pick state successfully immediately after adding new targets"
//            | NoAction ->
//                match targeted.Pick() with
//                | Some s -> GoFront s
//                | None ->
//                    match forward.Pick() with
//                    | Some s ->
//                        backward.RemoveBranch s
//                        GoFront s
//                    | None -> Stop

        override x.Reset () =
            forward.Reset()
            backward.Reset()
            targeted.Reset()

        override x.Remove cilState =
            forward.Remove cilState
            backward.Remove cilState
            targeted.Remove cilState

        override x.StatesCount with get() =
            forward.StatesCount + backward.StatesCount + targeted.StatesCount

type OnlyForwardSearcher(searcher : IForwardSearcher) =
    interface IBidirectionalSearcher with
        override x.Init cilStates _ = searcher.Init cilStates
        override x.Statuses() = []
        override x.Answer _ _ = ()
        override x.UpdatePobs _ _ = ()
        override x.UpdateStates parent children = searcher.Update(parent, children)
        override x.Pick () =
            match searcher.Pick() with
            | Some s -> GoFront s
            | None -> Stop
        override x.States() = searcher.States()
        override x.Reset() = searcher.Reset()
        override x.Remove cilState = searcher.Remove cilState
        override x.StatesCount with get() = searcher.StatesCount

// TODO: add init point searcher -- remove from in InitTarget(,)?
type OnlyBackwardSearcher(backwardSearcher : IBackwardSearcher, targetedSearcher : ITargetedSearcher) =
    interface IBidirectionalSearcher with
        override x.Init _ pobs = backwardSearcher.Init pobs
        override x.Statuses() = backwardSearcher.Statuses()
        override x.Answer pob status = backwardSearcher.Answer pob status
        override x.UpdatePobs pob newPob = backwardSearcher.Update pob newPob
        override x.UpdateStates parent children =
            let reached = targetedSearcher.Update parent children
            Seq.iter (backwardSearcher.AddBranch >> ignore) reached
        override x.Pick () =
            match backwardSearcher.Pick() with
            | Propagate(s, p) -> GoBack (s, p)
            | InitTargets(fromToS) ->
                fromToS |> Seq.iter targetedSearcher.AddTarget
                match targetedSearcher.Pick() with
                | Some s -> GoFront s
                | None -> internalfail "Targeted searcher must pick state successfully immediately after adding new targets"
            | NoAction ->
                match targetedSearcher.Pick() with
                | Some s -> GoFront s
                | None -> Stop

        // TODO
        override x.States() = Seq.empty
        override x.Reset() =
            backwardSearcher.Reset()
            targetedSearcher.Reset()
        override x.Remove cilState =
            backwardSearcher.Remove cilState
            targetedSearcher.Remove cilState
        // TODO
        override x.StatesCount with get() = 0

type BackwardSearcher(initPointSearcher : IInitPointSearcher) =
    let toPropagate = Dictionary<pob, System.Collections.Generic.Queue<cilState>>()
    let unansweredPobs = Dictionary<codeLocation, HashSet<pob>>()
    let statesByTarget = Dictionary<codeLocation, HashSet<cilState>>()
    let ancestorsOf = Dictionary<pob, HashSet<pob>>()
    
    let addPob (p : pob) =
        // Move to utils
        let mutable locationPobs = ref null
        if not <| unansweredPobs.TryGetValue(p.loc, locationPobs) then
            locationPobs <- ref (HashSet<pob>())
            unansweredPobs.[p.loc] <- locationPobs.Value
        locationPobs.Value.Add p |> ignore
        
        initPointSearcher.AddTarget p.loc
        
        if statesByTarget.ContainsKey p.loc then
            toPropagate.[p] <- statesByTarget.[p.loc] |> Queue
    
    // TODO: CPS
    let rec answerYes (s : cilState) (p : pob) =
        toPropagate.Remove p |> ignore
        unansweredPobs.[p.loc].Remove p |> ignore
        
        if unansweredPobs.[p.loc].Count = 0 then
            initPointSearcher.RemoveTarget p.loc
            unansweredPobs.Remove p.loc |> ignore
            Application.removeGoal p.loc
            
        let ancestors = if ancestorsOf.ContainsKey p then ancestorsOf.[p] |> seq else Seq.empty
        ancestors |> Seq.iter (answerYes s)
    
    interface IBackwardSearcher with    
        
        override x.Init pobs = pobs |> Seq.iter addPob

        override x.Update parent child =
            // Check for answered pobs?
            
            assert(unansweredPobs.ContainsKey(parent.loc) && unansweredPobs.[parent.loc].Contains(parent))
            
            let mutable ancestors = ref null
            if not <| ancestorsOf.TryGetValue(child, ancestors) then
                ancestors <- ref (HashSet<pob>())
                ancestorsOf.[child] <- ancestors.Value
            ancestors.Value.Add parent |> ignore
            
            addPob child

        override x.Answer pob status =
            match status with
            | Witnessed s' -> answerYes s' pob
            | _ -> __notImplemented__()
            
        override x.Statuses () = __notImplemented__()

        override x.Pick() =
            if toPropagate.Count > 0 then
                let pob = toPropagate.Keys |> Seq.head
                let states = toPropagate.[pob]
                let state = states.Dequeue()
                if states.Count = 0 then
                    toPropagate.Remove pob |> ignore
                Propagate(state, pob)
            else
                let initPoints = initPointSearcher.Pick()
                if Seq.isEmpty initPoints then NoAction
                else InitTargets initPoints

        override x.AddBranch cilState =
            match ipOperations.ip2codeLocation (CilStateOperations.currentIp cilState) with
            | None -> []
            | Some loc ->
                let cilState = CilStateOperations.deepCopy cilState
                
                let mutable locStates = ref null
                if not <| statesByTarget.TryGetValue(loc, locStates) then
                    locStates <- ref (HashSet<cilState>())
                    statesByTarget.[loc] <- locStates.Value
                locStates.Value.Add cilState |> ignore
                
                let pobsList = ref null
                if unansweredPobs.TryGetValue(loc, pobsList) then
                    for pob in pobsList.Value do
                        let mutable states = ref null
                        if not <| toPropagate.TryGetValue(pob, states) then
                            states <- ref (Queue<cilState>())
                            toPropagate.[pob] <- states.Value
                        states.Value.Enqueue cilState
                    pobsList.Value |> List.ofSeq
                else
                    []

        // TODO
        override x.Remove cilState = ()

        override x.Reset() =
            toPropagate.Clear()
            unansweredPobs.Clear()
            ancestorsOf.Clear()

        override x.StatesCount with get() = 0
