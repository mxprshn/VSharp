namespace VSharp.MethodSequences

open System
open VSharp
open VSharp.Core
open VSharp.Core.SolverInteraction
open VSharp.Interpreter.IL.CilState

type searchState = {
    condition : pathCondition
    model : model
    variables : variableId list
    sequence : methodSequence
    parent : searchState option
} with
    override x.ToString() =
        x.sequence.ToString()

module SearchState =
    let fromCilState (cilState : cilState) (stackToVar : stackToVar) =
        match cilState.entryMethod with
        | None -> internalfailf "Cannot create method sequence query from CilState without entry method"
        | Some entryMethod ->
            let mappingState = Substitutions.createMappingState entryMethod stackToVar []
            let wlp = Memory.WLP mappingState cilState.state.pc
            match SolverInteraction.checkSat wlp with
            | SmtUnsat _ | SmtUnknown _ ->
                internalfailf "Unsat WLP of initial state mapping"
            | SmtSat satInfo ->
                {
                    condition = wlp
                    model = satInfo.mdl
                    variables = PersistentDict.values stackToVar |> List.ofSeq
                    sequence = MethodSequence.createInitial entryMethod stackToVar
                    parent = None
                }

    let isComplete (state : searchState) =
        List.forall (fun (v : variableId) -> Utils.canBeCreatedBySolver v.typ) state.variables

    let rec getRootState (state : searchState) =
        match state.parent with
        | Some parent -> getRootState parent
        | None -> state
