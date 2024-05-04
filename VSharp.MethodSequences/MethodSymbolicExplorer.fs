namespace VSharp.MethodSequences

open System.Collections.Concurrent
open System.Collections.Generic
open VSharp
open VSharp.Core
open VSharp.Explorer
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilState
open VSharp.Utils

type IMethodSymbolicExplorer =
    abstract OnStates: IEvent<IMethod * cilState list>
    abstract OnExplorationFinished: IEvent<IMethod>

    abstract Enqueue: IMethod -> bool
    abstract member GetStates: IMethod -> cilState list option
    abstract member IsExplorationFinished: IMethod -> bool
    abstract member MakeStep: unit -> bool

type MethodSymbolicExplorer(createSearcher : unit -> IForwardSearcher) =
    let onStates = Event<IMethod * cilState list>()
    let onExplorationFinished = Event<IMethod>()

    let interpreter = ILInterpreter()

    let createSearcher = createSearcher

    let finishedStates = Dictionary<IMethod, ConcurrentBag<cilState>>()
    let finishedMethods = HashSet<IMethod>()
    let searchersQueue: IPriorityCollection<IMethod * IForwardSearcher, int> =
        BidictionaryPriorityQueue<IMethod * IForwardSearcher, int>()



    // TODO: this is copy-pasted from Explorer, unify
    let makeInitialCilStates (method : IMethod): cilState list =
        // TODO: subst type parameters
        let initialState = Memory.EmptyIsolatedState()
        // TODO: get rid of dirty cast
        let method = method :?> Method
        initialState.model <- Memory.EmptyModel method
        let declaringType = method.DeclaringType
        let cilState = cilState.CreateMethodSequenceInitial method initialState
        let this =
            if method.HasThis then
                if Types.IsValueType declaringType then
                    Memory.NewStackFrame initialState None []
                    Memory.AllocateTemporaryLocalVariableOfType initialState "this" 0 declaringType |> Some
                else
                    let this = Memory.MakeSymbolicThis initialState method
                    !!(IsNullReference this) |> AddConstraint initialState
                    Some this
            else None
        let parameters = SVMExplorer.AllocateByRefParameters initialState method
        Memory.InitFunctionFrame initialState method this (Some parameters)
        match this with
        | Some this -> SolveThisType initialState this
        | _ -> ()
        let cilStates = ILInterpreter.CheckDisallowNullAttribute method None cilState false id
        assert(List.length cilStates = 1)
        if not method.IsStaticConstructor then
            let cilState = List.head cilStates
            interpreter.InitializeStatics cilState declaringType List.singleton
        else
            Memory.MarkTypeInitialized initialState declaringType
            cilStates

    let queue (method : IMethod) =
        if finishedStates.ContainsKey method then
            false
        else
            let newSearcher = createSearcher()
            let initialStates = makeInitialCilStates method
            newSearcher.Init initialStates
            searchersQueue.Insert (method, newSearcher) 0
            finishedStates[method] <- ConcurrentBag<cilState>()
            true

    let getStates (method : IMethod) =
        let mutable methodFinishedStates = ref null
        if finishedStates.TryGetValue(method, methodFinishedStates) then
            List.ofSeq methodFinishedStates.Value |> Some
        else None

    let rec makeStep() =
        match searchersQueue.Choose() with
        | None -> false
        | Some(method, searcher) ->
            match searcher.Pick() with
            | Some cilState ->
                try
                    let goodStates, _, _ = interpreter.ExecuteOneInstruction cilState
                    let goodStates, finished = goodStates |> List.partition (fun s -> s.IsExecutable || s.IsIsolated)
                    let mutable currentStateIsStopped = false
                    let newStates =
                        match goodStates with
                        | s::goodStates when LanguagePrimitives.PhysicalEquality cilState s ->
                            goodStates
                        | _ ->
                            currentStateIsStopped <- true
                            goodStates
                    searcher.Update(cilState, newStates)
                    if currentStateIsStopped then
                        searcher.Remove cilState
                    let currentSearcherPriority = searchersQueue.TryGetPriority(method, searcher).Value
                    searchersQueue.Update (method, searcher) (currentSearcherPriority + 1)
                    match finished with
                    | [] -> ()
                    | _ ->
                        for finishedState in finished do
                            finishedStates[method].Add finishedState
                        onStates.Trigger(method, finished)
                    true
                with
                | e ->
                    Logger.warning $"Removing state because of {e.ToString()}"
                    searcher.Remove cilState
                    makeStep()
            | None ->
                searchersQueue.Remove(method, searcher)
                finishedMethods.Add method |> ignore
                onExplorationFinished.Trigger(method)
                makeStep()

    let isExplorationFinished method = finishedMethods.Contains method

    interface IMethodSymbolicExplorer with
        member x.OnStates = onStates.Publish
        member x.OnExplorationFinished = onExplorationFinished.Publish
        member x.Enqueue method = queue method
        member x.GetStates method = getStates method
        member x.IsExplorationFinished method = isExplorationFinished method
        member x.MakeStep() = makeStep()
