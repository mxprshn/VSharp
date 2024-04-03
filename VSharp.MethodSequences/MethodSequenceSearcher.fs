namespace VSharp.MethodSequences

open System
open System.Collections.Generic
open VSharp
open VSharp.Core
open VSharp.Explorer
open VSharp.Interpreter.IL.CilState

type prependCilStateResult =
    | Unsat
    | Sat of searchState

type searchAction =
    | PrependCilState of cilState * int
    
with
   override x.ToString() = "SearchAction"

type searchStackFrame = {
    id : uint
    searchState : searchState
    varSubsts : Dictionary<IMethod, varSubst list>
    actions : LinkedList<searchAction>
} with

    override x.ToString() =
        $"[#{x.id}|{x.searchState.ToString()}]"
      
type searchStepResult =
    | NoMoreSteps
    | Continue
    | SequenceFound of methodSequence

type MethodSequenceSearcher(targetState : cilState) =
    let stack = LinkedList<searchStackFrame>()
    let framesByMethods = Dictionary<IMethod, List<searchStackFrame>>()
    
    let targetState = targetState
    
    let methodExplorer: IMethodSymbolicExplorer = MethodSymbolicExplorer(fun () -> ExecutionTreeSearcher(Some 42))
    
    let mutable actionCount = 0
    let mutable currentStackFrameId = 0u
    
    let getNextStackFrameId() =
        let idToReturn = currentStackFrameId
        currentStackFrameId <- currentStackFrameId + 1u
        idToReturn
    
    let checkSetter setter varId (methodSequenceElement : methodSequenceElement) =
        match methodSequenceElement with
        | Call(method, _, Some this, _) when method = setter && this = varId -> true
        | _ -> false
        
    let wasSetterCalled searchState varId setter =
        List.exists (checkSetter setter varId) searchState.sequence.elements
        
    let hasCilStateTouchedFields (searchState : searchState) (varSubst : varSubst) (cilState : cilState) =
        let constantsInCondition = Terms.GetConstants (PathConditionToSeq searchState.condition)
        let fieldsInCondition = HashSet<fieldId>()
        for constant in constantsInCondition do
            match constant.term with
            | Constant(_, source, _) ->
                match Utils.getFieldsFromConstant source with
                | [] -> ()
                | fields -> for f in fields do fieldsInCondition.Add f |> ignore
            | _ -> ()
        let touchedFields =
            match cilState.methodSequenceStats with
            | Some stats ->
                PersistentDict.values stats.touchedFields |> Seq.fold PersistentSet.union PersistentSet.empty |> PersistentSet.toSeq |> Seq.map Reflection.wrapField
            | None -> Seq.empty
        Seq.exists fieldsInCondition.Contains touchedFields
    
    let newFrame (searchState : searchState) = 
        let varSubsts = Dictionary<IMethod, varSubst list>()
        let cilStateList = LinkedList<searchAction>()
        let frame = { id = getNextStackFrameId(); searchState = searchState; varSubsts = varSubsts; actions = cilStateList }
        
        let visitMethod (method : IMethod) (substs : varSubst list) (filterCilState : cilState -> bool) =
            let mutable methodFrames = ref null
            if framesByMethods.TryGetValue(method, methodFrames) then
                methodFrames.Value.Add frame
            else
                let methodFrames = List()
                methodFrames.Add frame
                framesByMethods[method] <- methodFrames
            
            varSubsts[method] <- substs
            match methodExplorer.GetStates method with
            | Some cilStates ->
                cilStates |> Seq.filter filterCilState |> Seq.map (fun s -> LinkedListNode(PrependCilState(s, 0))) |> Seq.iter cilStateList.AddFirst
            | None ->
                methodExplorer.Enqueue method |> ignore

        let primitiveVariables = searchState.variables |> Seq.groupBy _.typ |> Seq.filter (fun (t, _) -> not <| Utils.canBeCreatedBySolver t) |> Seq.toList

        for typ, variables in primitiveVariables do
            for variable in variables do
                for setter in Utils.getPropertySetters typ |> Seq.filter (not << wasSetterCalled searchState variable) do
                    let subst = PersistentDict.ofSeq [ variable, StackVal(ThisKey setter) ]
                    visitMethod setter [ subst ] (hasCilStateTouchedFields searchState subst) 
        
        for typ, variables in primitiveVariables do
            let substTargets = Utils.nonEmptySubsets (Seq.toList variables) |> List.filter (not << List.isEmpty)
            // We can substitute return value of the method to any subset of existing variables
            for ctor in typ.GetConstructors() |> Seq.map Application.getMethod |> Seq.sortByDescending Utils.getNonTrivialParametersCount do
                let wrapper = MethodWrappers.getConstructorWrapper ctor
                let substs = List<varSubst>()
                for substTarget in substTargets do
                    let subst = List.map (fun target -> target, RetVal) substTarget |> PersistentDict.ofSeq
                    substs.Add subst
                visitMethod wrapper (Seq.toList substs) (fun _ -> true)
                
        frame

    let onCilStatesExplored (method : IMethod, cilStates : cilState list) =
        // TODO: find constants from state once
        for frame in framesByMethods[method] do
            let filterState cilState =
                if Utils.isSetter method then
                    let subst = frame.varSubsts[method].Head
                    hasCilStateTouchedFields frame.searchState subst cilState
                else
                    true
                
            // TODO: some better logic
            for cilState in cilStates |> Seq.filter filterState |> Seq.map (fun s -> LinkedListNode(PrependCilState(s, 0))) do
                frame.actions.AddLast cilState
                actionCount <- actionCount + 1
            
            
    let isPrependMethodCall (method : IMethod) action =
        match action with
        | PrependCilState(cilState, _) -> cilState.entryMethod.Value :> IMethod = method
            
    let onExplorationFinished (method : IMethod) =
        let framesToRemove = List()
        let framesByMethod = framesByMethods[method]
        for frame in framesByMethod do
            if not <| Seq.exists (isPrependMethodCall method) frame.actions then 
                frame.varSubsts.Remove method |> ignore
            if frame.varSubsts.Count = 0 then
                framesToRemove.Add frame
        for frameToRemove in framesToRemove do
            stack.Remove frameToRemove |> ignore
            framesByMethod.Remove frameToRemove |> ignore
        
    do
        methodExplorer.OnStates.Add onCilStatesExplored
        methodExplorer.OnExplorationFinished.Add onExplorationFinished
        let stackToVar = Substitutions.mapThisAndParametersToVars targetState.entryMethod.Value
        let searchState = SearchState.fromCilState targetState stackToVar
        let newFrame = newFrame searchState
        stack.AddFirst(LinkedListNode(newFrame))
        actionCount <- actionCount + newFrame.actions.Count
        
    let prependState (searchState : searchState) (cilState : cilState) (subst : varSubst) =
        let getRetValTerm() =
            match EvaluationStack.Length cilState.state.memory.EvaluationStack with
            | 0 -> None
            | 1 ->
                let result, _ = EvaluationStack.Pop cilState.state.memory.EvaluationStack
                assert cilState.entryMethod.IsSome
                let method = cilState.entryMethod.Value
                let typ = if method.IsConstructor then method.DeclaringType else method.ReturnType
                Some <| Types.Cast result typ
            | _ -> __unreachable__()
        
        let retValTerm = lazy getRetValTerm()
    
        let getSourceTerm (varSubstSource : varSubstSource) =
            match varSubstSource with
            | RetVal ->
                match retValTerm.Value with
                | Some retValTerm -> retValTerm
                | None -> __unreachable__()
            | StackVal key ->
                
                Memory.ReadLocalVariable cilState.state key
                
        let method = cilState.entryMethod.Value
        let mappingFrame = List<stackKey * term option * Type>()
        
        for variable in searchState.variables do
            let toKey = Variables.stackKeyOf variable
            let typ = variable.typ
            let term = Option.map getSourceTerm (PersistentDict.tryFind subst variable)
            mappingFrame.Add (toKey, term, typ)
 
        Memory.NewStackFrame cilState.state None (List.ofSeq mappingFrame)
        
        let wlp = Memory.WLP cilState.state searchState.condition
        
        if IsFalsePathCondition wlp then
            Memory.PopFrame cilState.state
            Unsat
        else
            match SolverInteraction.checkSat wlp with
            | SolverInteraction.SmtUnsat _ | SolverInteraction.SmtUnknown _ ->
                Memory.PopFrame cilState.state
                Unsat
            | SolverInteraction.SmtSat satInfo -> 
                let stackToVar = Substitutions.mapThisAndParametersToVars method
                let substitutedVars = PersistentDict.keys subst
                let remainingVars = List.filter (fun v -> not <| Seq.contains v substitutedVars) searchState.variables
                let mappingState = Substitutions.createMappingState method stackToVar remainingVars
                // TODO: check case when stackToVar is empty
                let wlp = Memory.WLP mappingState wlp
                let newState = {
                    condition = wlp
                    model = satInfo.mdl
                    variables = remainingVars @ (PersistentDict.values stackToVar |> Seq.toList)
                    sequence = MethodSequence.addCall searchState.sequence method stackToVar subst
                    parent = Some searchState
                }
                
                Memory.PopFrame cilState.state
                Sat newState
                
    let tryRemoveFrame (frame : searchStackFrame) =
        let methodsToRemove = Seq.filter methodExplorer.IsExplorationFinished frame.varSubsts.Keys
        for methodToRemove in methodsToRemove do
            if not <| Seq.exists (isPrependMethodCall methodToRemove) frame.actions then 
                frame.varSubsts.Remove methodToRemove |> ignore
        if frame.varSubsts.Count = 0 then
            stack.Remove frame |> ignore
            for method in frame.varSubsts.Keys do
                framesByMethods[method].Remove frame |> ignore

    
    let rec makeStep (startFromFrame : LinkedListNode<searchStackFrame>) : searchStepResult =
        if stack.Count = 0 then
            NoMoreSteps
        else
            let mutable currentFrameNode = startFromFrame
            while currentFrameNode.Value.actions.Count = 0 && currentFrameNode.Next <> null do
                let temp = currentFrameNode.Value
                currentFrameNode <- currentFrameNode.Next
                tryRemoveFrame temp
            if currentFrameNode.Value.actions.Count = 0 && currentFrameNode.Next = null then
                tryRemoveFrame currentFrameNode.Value
                NoMoreSteps
            else
                let currentFrame = currentFrameNode.Value
                let action = currentFrame.actions.First
                currentFrame.actions.RemoveFirst()
                actionCount <- actionCount - 1
                match action.Value with
                | PrependCilState(cilState, substIndex) ->
                    let method = cilState.entryMethod.Value
                    let methodSubstsCount = currentFrame.varSubsts[method].Length
                    if substIndex >= methodSubstsCount then
                        makeStep currentFrameNode
                    else
                        if substIndex < methodSubstsCount - 1 then
                            currentFrame.actions.AddFirst(LinkedListNode(PrependCilState(cilState, substIndex + 1)))
                            actionCount <- actionCount + 1
                        let varSubst = currentFrame.varSubsts[method][substIndex]
                        match prependState currentFrame.searchState cilState varSubst with
                        | Unsat -> Continue
                        | Sat newState ->
                            let newFrame = newFrame newState
                            stack.AddBefore(currentFrameNode, LinkedListNode(newFrame))
                            actionCount <- actionCount + newFrame.actions.Count
                            if SearchState.isComplete newState then
                                
                                SequenceFound newState.sequence
                            else
                                Continue
    let mutable makeExplorerStep = true
    member x.MakeStep() =
        makeExplorerStep <- not makeExplorerStep
        let result = List()
        if makeExplorerStep || actionCount = 0 then // Don't do that if there is nothing to explore
            methodExplorer.MakeStep() |> ignore
            result
        else
            match makeStep stack.First with
            | Continue | NoMoreSteps -> result
            | SequenceFound seq ->
                result.Add seq
                result