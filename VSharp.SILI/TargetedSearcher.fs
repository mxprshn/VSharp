namespace VSharp.Interpreter.IL

open System.Collections.Generic

open VSharp
open VSharp.Interpreter.IL
open VSharp.Utils
open CilStateOperations

type TargetedSearcher(maxBound, target) =
    inherit WeightedSearcher(maxBound, ShortestDistanceWeighter(target), BidictionaryPriorityQueue())

    let isStopped s = isStopped s || violatesLevel s maxBound

    override x.Insert states =
        base.Insert states
        for state in states do
            match x.TryGetWeight state with
            | None when not state.suspended ->
                removeTarget state target
            | _ -> ()

    override x.Update (parent, newStates) =
        let needsUpdating state =
            let optCurrLoc = tryCurrentLoc state
            let onVertex optLoc =
                match optLoc with
                | Some loc ->
                    let cfg = loc.method.CFG
                    cfg.IsBasicBlockStart loc.offset
                | None -> false

            isStopped state || onVertex optCurrLoc

        if needsUpdating parent then
            base.Update (parent, newStates)
        else
            base.Insert newStates

        for state in Seq.append [parent] newStates do
            match x.TryGetWeight state with
            | None when not state.suspended ->
                removeTarget state target
            | _ -> ()

    member x.TargetedInsert states : cilState list =
        x.Insert states
        states |> Seq.fold (fun reachedStates state ->
        match x.TryGetWeight state with
        | Some 0u -> state::reachedStates
        | _ -> reachedStates) []

    member x.TargetedUpdate (parent, newStates) =
        x.Update (parent, newStates)
        Seq.append [parent] newStates |> Seq.fold (fun reachedStates state ->
        match x.TryGetWeight state with
        | Some 0u -> state::reachedStates
        | _ -> reachedStates) []

type ITargetManager =
    abstract member CalculateTarget : cilState -> codeLocation option
    abstract member IsStuck : cilState -> bool

type RecursionBasedTargetManager(statistics : SILIStatistics, threshold : uint) =
    interface ITargetManager with
        override x.CalculateTarget state =
            let locStack = state.ipStack |> Seq.choose ipOperations.ip2codeLocation
            let inCoverageZone loc = loc.method.InCoverageZone
            Cps.Seq.foldlk (fun reachingLoc loc k ->
            match reachingLoc with
            | None when inCoverageZone loc ->
                let localHistory = Seq.filter inCoverageZone (history state)
                match statistics.PickUnvisitedWithHistoryInCFG(loc, localHistory) with
                | None -> k None
                | Some l -> Some l
            | _ -> k reachingLoc) None locStack id

        override x.IsStuck state =
            let optCurrLoc = tryCurrentLoc state
            match optCurrLoc with
            | Some currLoc ->
                let cfg = currLoc.method.CFG
                let onVertex = cfg.IsBasicBlockStart currLoc.offset
                let level = if PersistentDict.contains currLoc state.level then state.level.[currLoc] else 0u
                onVertex && level > threshold
            | _ -> false

type VisitingBasedTargetManager(statistics : SILIStatistics) =
    interface ITargetManager with
        override x.CalculateTarget state =
            let locStack = state.ipStack |> Seq.choose ipOperations.ip2codeLocation
            let inCoverageZone loc = loc.method.InCoverageZone
            Cps.Seq.foldlk (fun reachingLoc loc k ->
            match reachingLoc with
            | None when inCoverageZone loc ->
                match statistics.PickTotalUnvisitedInMethod(loc) with
                | None -> k None
                | Some l -> Some l
            | _ -> k reachingLoc) None locStack id

        override x.IsStuck state =
            let optCurrLoc = tryCurrentLoc state
            match optCurrLoc with
            | Some currLoc ->
                let cfg = currLoc.method.CFG
                let onVertex = cfg.IsBasicBlockStart currLoc.offset
                let covered = statistics.IsCovered currLoc
                onVertex && covered
            | _ -> false

type GuidedSearcher(maxBound, baseSearcher : IForwardSearcher, targetCalculator : ITargetManager) =
    let targetedSearchers = Dictionary<codeLocation, TargetedSearcher>()
    let getTargets (state : cilState) = state.targets

    let calculateTarget (state : cilState): codeLocation option =
        targetCalculator.CalculateTarget state
    let isStuck (state : cilState): bool =
        targetCalculator.IsStuck state

    let mkTargetedSearcher target = TargetedSearcher(maxBound, target)
    let getTargetedSearcher target =
        Dict.getValueOrUpdate targetedSearchers target (fun () -> mkTargetedSearcher target)

    let mutable index = 1

    let insertInTargetedSearcher state target =
        let targetedSearcher = getTargetedSearcher target
        targetedSearcher.Insert [state]

    let addReturnTarget state =
        let startingLoc = startingLoc state
        let startingMethod = startingLoc.method
        let cfg = startingMethod.CFG

        for retOffset in cfg.Sinks do
            let target = {offset = retOffset; method = startingMethod}

            match state.targets with
            | Some targets ->
                state.targets <- Some <| Set.add target targets
                if not <| Set.contains target targets then
                    insertInTargetedSearcher state target
            | None ->
                state.targets <-Some (Set.add target Set.empty)
                insertInTargetedSearcher state target

    let deleteTargetedSearcher target =
        let targetedSearcher = getTargetedSearcher target
        for state in targetedSearcher.ToSeq () do
            removeTarget state target
        targetedSearchers.Remove target |> ignore

    let insertInTargetedSearchers states =
        states
     |> Seq.iter (fun state ->
        option {
            let! sTargets = getTargets state
            sTargets
         |> Seq.iter (fun target ->
            insertInTargetedSearcher state target)
        } |> ignore)

    let updateTargetedSearchers parent (newStates : cilState seq) =
        let addedCilStates = Dictionary<codeLocation, cilState list>()
        let updateParentTargets = getTargets parent

        match updateParentTargets with
        | Some targets ->
            for target in targets do
                addedCilStates.Add(target, [])
        | None -> ()

        for state in newStates do
            option {
                let! sTargets = getTargets state
                sTargets |> Seq.iter (fun target ->
                let targets = Dict.getValueOrUpdate addedCilStates target (fun () -> [])
                addedCilStates.[target] <- state :: targets)
            } |> ignore

        let reachedTargets =
            addedCilStates |> Seq.fold (fun reached kvpair ->
            let targetedSearcher = getTargetedSearcher kvpair.Key
            let reachedStates =
                match updateParentTargets with
                | Some targets when targets.Contains kvpair.Key ->
                    targetedSearcher.TargetedUpdate (parent, kvpair.Value)
                | _ -> targetedSearcher.TargetedInsert addedCilStates.[kvpair.Key]

            if not <| List.isEmpty reachedStates then
                for state in reachedStates do
                    addReturnTarget state |> ignore
                kvpair.Key :: reached
            else reached) List.empty

        Seq.iter (fun target -> deleteTargetedSearcher target) reachedTargets

    let update parent newStates =
        baseSearcher.Update (parent, newStates)
        updateTargetedSearchers parent newStates

    let suspend (state : cilState) : unit =
        state.suspended <- true
        update state Seq.empty

    let setTargetOrSuspend state =
        match calculateTarget state with
        | Some target ->
            addTarget state target
            insertInTargetedSearcher state target
        | None ->
            state.targets <- None
            suspend state

    let rec pick' k =
        let pickFromBaseSearcher k =
            match baseSearcher.Pick() with
            | Some state ->
                match state.targets with
                | None when isStuck state ->
                    setTargetOrSuspend state
                    pick' k
                | _ -> k <| Some state
            | None -> k None
        let pickFromTargetedSearcher k =
            let targetSearcher = targetedSearchers |> Seq.item index
            let optState = targetSearcher.Value.Pick()

            if Option.isSome optState then k <| optState
            else
                deleteTargetedSearcher targetSearcher.Key
                pick' k
        let size = targetedSearchers.Count

        index <- (index + 1) % (size + 1)
        if index <> size
            then pickFromTargetedSearcher k
            else pickFromBaseSearcher k

    let pick () = pick' id

    interface IForwardSearcher with
        override x.Init states =
            baseSearcher.Init states
            insertInTargetedSearchers states
        override x.Pick() = pick ()
        override x.Update (parent, newStates) = update parent newStates
        override x.States() =
            seq {
                yield baseSearcher.States()
                yield! targetedSearchers |> Seq.map (fun kvp -> (kvp.Value :> IForwardSearcher).States())
            } |> Seq.concat

type ShortestDistanceBasedSearcher(maxBound, statistics : SILIStatistics) =
    inherit SampledWeightedSearcher(maxBound, IntraproceduralShortestDistanceToUncoveredWeighter(statistics))
