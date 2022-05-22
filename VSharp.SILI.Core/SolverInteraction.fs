namespace VSharp.Core

open System
open System.Collections.Generic
open FSharpx.Collections
open VSharp

module public SolverInteraction =

    type unsatCore() = class end

    type encodingContext =
        { addressOrder : Map<concreteHeapAddress, int>}

    type satInfo = { mdl : model; usedPaths : path seq }
    type unsatInfo = { core : term[] }

    type smtResult =
        | SmtSat of satInfo
        | SmtUnsat of unsatInfo
        | SmtUnknown of string

    type ISolver =
        abstract CheckSat : encodingContext -> query -> smtResult
        abstract Assert : encodingContext -> level -> formula -> unit
        abstract AddPath : encodingContext -> path -> unit
        abstract CheckAssumptions : encodingContext -> model -> formula seq -> smtResult

    let mutable private solver : ISolver option = None

    let configureSolver s = solver <- Some s

    let getEncodingContext (state : state) =
        let addresses = PersistentDict.keys state.allocatedTypes
        let sortedAddresses = Seq.sortWith VectorTime.compare addresses
        let order = Seq.fold (fun (map, i) address -> Map.add address i map, i + 1) (Map.empty, 1) sortedAddresses |> fst
        let orderWithNull = Map.add VectorTime.zero 0 order
        { addressOrder = orderWithNull }
        
    let private checkSatPlainly state =
        let ctx = getEncodingContext state
        let formula = state.pc.ToSeq() |> conjunction
        match solver with
        | Some s ->
            let model = 
                state.model
                |> Option.defaultValue { state = State.makeEmpty None; subst = Dictionary<_, _>(); complete = true }
            s.CheckSat ctx { lvl = Level.zero; queryFml = formula; currentModel = model }
        | None -> SmtUnknown "Solver not configured"
 
    let private checkSatIncrementally state =
        let ctx = getEncodingContext state
        let conditions = state.pc |> PC.toSeq
        match solver with
        | Some s ->
            let model = 
                state.model
                |> Option.defaultValue { state = State.makeEmpty None; subst = Dictionary<_, _>(); complete = true }
            s.CheckAssumptions ctx model conditions
        | None -> SmtUnknown "Solver not configured"
        
    let checkSat state =
        // TODO: need to solve types here? #do
        if FeatureFlags.current.isIncrementalityEnabled then
            checkSatIncrementally state
        else
            checkSatPlainly state