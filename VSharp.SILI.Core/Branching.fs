namespace VSharp.Core

open VSharp

module Branching =
    let checkSat state condition = TypeSolver.checkSatWithSubtyping state condition

    let commonGuardedStatedApplyk f state term mergeResults k =
        match term.term with
        | Union gvs ->
            let filterUnsat (g, v) k =
                let pc = PC.add state.pc g
                if PC.isFalse pc then k None
                else Some (pc, v) |> k
            Cps.List.choosek filterUnsat gvs (fun pcs ->
            match pcs with
            | [] -> k []
            | (pc, v)::pcs ->
                let copyState (pc, v) k = f (Memory.copy state pc) v k
                Cps.List.mapk copyState pcs (fun results ->
                    state.pc <- pc
                    f state v (fun r ->
                    r::results |> mergeResults |> k)))
        | _ -> f state term (List.singleton >> k)
    let guardedStatedApplyk f state term k = commonGuardedStatedApplyk f state term Memory.mergeResults k
    let guardedStatedApply f state term = guardedStatedApplyk (Cps.ret2 f) state term id

    let guardedStatedMap mapper state term =
        commonGuardedStatedApplyk (fun state term k -> mapper state term |> k) state term id id

    let mutable branchesReleased = false

    let commonStatedConditionalExecutionk (state : state) conditionInvocation thenBranch elseBranch merge2Results k =
        let execution thenState elseState condition k =
            assert (condition <> True && condition <> False)
            thenBranch thenState (fun thenResult ->
            elseBranch elseState (fun elseResult ->
            merge2Results thenResult elseResult |> k))
        conditionInvocation state (fun (condition, conditionState) ->
        let pc = state.pc
        assert(PC.toSeq pc |> conjunction |> state.model.Eval |> isTrue)
        let evaled = state.model.Eval condition

        let mutable thenMethodSequence : methodSequence option = None
        let mutable elseMethodSequence : methodSequence option = None
        match state.methodSequence with
        | Some s ->
            let evaled = s.Eval condition
            if isTrue evaled then
                thenMethodSequence <- state.methodSequence
            elif isFalse evaled then
                elseMethodSequence <- state.methodSequence
        | None -> ()

        if isTrue evaled then
            let notCondition = !!condition
            assert(state.model.Eval notCondition |> isFalse)
            let elsePc = PC.add pc notCondition
            if PC.isFalse elsePc then
                conditionState.methodSequence <- thenMethodSequence
                thenBranch conditionState (List.singleton >> k)
            elif not branchesReleased then
                conditionState.pc <- elsePc
                match checkSat conditionState notCondition with
                | SolverInteraction.SmtUnsat _ ->
                    conditionState.pc <- pc
                    TypeSolver.refineTypes conditionState condition
                    conditionState.methodSequence <- thenMethodSequence
                    thenBranch conditionState (List.singleton >> k)
                | SolverInteraction.SmtUnknown _ ->
                    conditionState.pc <- PC.add pc condition
                    TypeSolver.refineTypes conditionState condition
                    conditionState.methodSequence <- thenMethodSequence
                    thenBranch conditionState (List.singleton >> k)
                | SolverInteraction.SmtSat model ->
                    let thenState = conditionState
                    let elseState = Memory.copy conditionState elsePc
                    elseState.model <- model.mdl
                    thenState.pc <- PC.add pc condition
                    TypeSolver.refineTypes thenState condition
                    thenState.methodSequence <- thenMethodSequence
                    elseState.methodSequence <- elseMethodSequence
                    execution thenState elseState condition k
            else
                conditionState.pc <- PC.add pc condition
                conditionState.methodSequence <- thenMethodSequence
                thenBranch conditionState (List.singleton >> k)
        elif isFalse evaled then
            let notCondition = !!condition
            assert(state.model.Eval notCondition |> isTrue)
            let thenPc = PC.add state.pc condition
            if PC.isFalse thenPc then
                conditionState.methodSequence <- elseMethodSequence
                elseBranch conditionState (List.singleton >> k)
            elif not branchesReleased then
                conditionState.pc <- thenPc
                match checkSat conditionState condition with
                | SolverInteraction.SmtUnsat _ ->
                    conditionState.pc <- pc
                    TypeSolver.refineTypes conditionState notCondition
                    conditionState.methodSequence <- elseMethodSequence
                    elseBranch conditionState (List.singleton >> k)
                | SolverInteraction.SmtUnknown _ ->
                    conditionState.pc <- PC.add pc notCondition
                    TypeSolver.refineTypes conditionState notCondition
                    conditionState.methodSequence <- elseMethodSequence
                    elseBranch conditionState (List.singleton >> k)
                | SolverInteraction.SmtSat model ->
                    let thenState = conditionState
                    let elseState = Memory.copy conditionState (PC.add pc notCondition)
                    thenState.model <- model.mdl
                    TypeSolver.refineTypes elseState notCondition
                    thenState.methodSequence <- thenMethodSequence
                    elseState.methodSequence <- elseMethodSequence
                    execution thenState elseState condition k
            else
                conditionState.pc <- PC.add pc notCondition
                conditionState.methodSequence <- elseMethodSequence
                elseBranch conditionState (List.singleton >> k)
        else __unreachable__())

    let statedConditionalExecutionWithMergek state conditionInvocation thenBranch elseBranch k =
        commonStatedConditionalExecutionk state conditionInvocation thenBranch elseBranch Memory.merge2Results k
    let statedConditionalExecutionWithMerge state conditionInvocation thenBranch elseBranch =
        statedConditionalExecutionWithMergek state conditionInvocation thenBranch elseBranch id
