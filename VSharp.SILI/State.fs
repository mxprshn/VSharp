﻿namespace VSharp
open VSharp.Utils
open VSharp.Types

module internal State =
    module SymbolicHeap = Heap

    type internal stack = MappedStack.stack<StackKey, MemoryCell<Term>>
    type internal pathCondition = Term list
    type internal entry = { key : StackKey; mtd : TermMetadata; typ : TermType option }
    type internal stackFrame = { func : (FunctionIdentifier * pathCondition) option; entries : list<entry> ; time : Timestamp }
    type internal frames = { f : Stack.stack<stackFrame>; sh : StackHash }
    type internal GeneralizedHeap =
        | Defined of SymbolicHeap
        | HigherOrderApplication of Term * ConcreteHeapAddress * Timestamp
        | RecursiveApplication of FunctionIdentifier * ConcreteHeapAddress * Timestamp
        | Composition of state * GeneralizedHeap
        | Merged of (Term * GeneralizedHeap) list
    and internal heap = GeneralizedHeap
    and internal staticMemory = GeneralizedHeap
    and internal state = { stack : stack; heap : heap; statics : staticMemory; frames : frames; pc : pathCondition }

// ------------------------------- Primitives -------------------------------

    let internal empty : state = {
        stack = MappedStack.empty;
        heap = Defined SymbolicHeap.empty;
        statics = Defined SymbolicHeap.empty;
        frames = { f = Stack.empty; sh = List.empty };
        pc = List.empty
    }

    type internal 'a SymbolicValue =
        | Specified of 'a
        | Unspecified

    let internal zeroTime : Timestamp = System.UInt32.MinValue

    let internal nameOfLocation = term >> function
        | HeapRef(((_, t), []), _) -> toString t
        | StackRef((name, _), []) -> name
        | StaticRef(name, []) -> System.Type.GetType(name).FullName
        | HeapRef((_, path), _)
        | StackRef(_, path)
        | StaticRef(_, path) -> path |> Seq.map (fst >> toString) |> join "."
        | l -> "requested name of an unexpected location " + (toString l) |> internalfail

    let internal readStackLocation (s : state) key = MappedStack.find key s.stack
    let internal readHeapLocation (h : SymbolicHeap) key = h.[key] |> fst3

    let internal isAllocatedOnStack (s : state) key = MappedStack.containsKey key s.stack
    let internal staticMembersInitialized (s : state) typeName =
        match s.statics with
        | Defined h ->  SymbolicHeap.contains (Terms.MakeStringKey typeName) h
        | _ -> __notImplemented__()

    let internal newStackFrame time metadata (s : state) funcId frame : state =
        let pushOne (map : stack) (key, value, typ) =
            match value with
            | Specified term -> { key = key; mtd = metadata; typ = typ }, MappedStack.push key (term, time, time) map
            | Unspecified -> { key = key; mtd = metadata; typ = typ }, MappedStack.reserve key map
        in
        let frameMetadata = Some(funcId, s.pc) in
        let locations, newStack = frame |> List.mapFold pushOne s.stack in
        let f' = Stack.push s.frames.f { func = frameMetadata; entries = locations; time = time } in
        let sh' = frameMetadata.GetHashCode()::s.frames.sh in
        { s with stack = newStack; frames = {f = f'; sh = sh'} }

    let internal newScope time metadata (s : state) frame : state =
        let pushOne (map : stack) (key, value, typ) =
            match value with
            | Specified term -> { key = key; mtd = metadata; typ = typ }, MappedStack.push key (term, time, time) map
            | Unspecified -> { key = key; mtd = metadata; typ = typ }, MappedStack.reserve key map
        in
        let locations, newStack = frame |> List.mapFold pushOne s.stack in
        { s with stack = newStack; frames = { s.frames with f = Stack.push s.frames.f { func = None; entries = locations; time = time } } }

    let internal pushToCurrentStackFrame (s : state) key value = MappedStack.push key value s.stack
    let internal popStack (s : state) : state =
        let popOne (map : stack) entry = MappedStack.remove map entry.key
        let { func = metadata; entries = locations; time = _ } = Stack.peek s.frames.f in
        let f' = Stack.pop s.frames.f in
        let sh = s.frames.sh in
        let sh' =
            match metadata with
            | Some _ ->
                assert(not <| List.isEmpty sh)
                List.tail sh
            | None -> sh
        { s with stack = List.fold popOne s.stack locations; frames = { f = f'; sh = sh'} }

    let internal writeStackLocation (s : state) key value : state =
        { s with stack = MappedStack.add key value s.stack }

    let internal stackFold = MappedStack.fold

    let inline private entriesOfFrame f = f.entries
    let inline private keyOfEntry en = en.key

    let internal frameTime (s : state) key =
        match List.tryFind (entriesOfFrame >> List.exists (keyOfEntry >> ((=) key))) s.frames.f with
        | Some { func = _; entries = _; time = t} -> t
        | None -> internalfailf "stack does not contain key %O!" key

    let internal typeOfStackLocation (s : state) key =
        let forMatch = List.tryPick (entriesOfFrame >> List.tryPick (fun { key = l; mtd = _; typ = t } -> if l = key then Some t else None)) s.frames.f
        match forMatch with
        | Some (Some t) -> t
        | Some None -> internalfailf "unknown type of stack location %O!" key
        | None -> internalfailf "stack does not contain key %O!" key

    let internal metadataOfStackLocation (s : state) key =
        match List.tryPick (entriesOfFrame >> List.tryPick (fun { key = l; mtd = m; typ = _ } -> if l = key then Some m else None)) s.frames.f with
        | Some t -> t
        | None -> internalfailf "stack does not contain key %O!" key

    let internal compareStacks s1 s2 = MappedStack.compare (fun key value -> StackRef Metadata.empty key []) fst3 s1 s2

    let internal withPathCondition (s : state) cond : state = { s with pc = cond::s.pc }
    let internal popPathCondition (s : state) : state =
        match s.pc with
        | [] -> internalfail "cannot pop empty path condition"
        | _::p' -> { s with pc = p' }

    let private stackOf (s : state) = s.stack
    let internal heapOf (s : state) = s.heap
    let internal staticsOf (s : state) = s.statics
    let private framesOf (s : state) = s.frames
    let private framesHashOf (s : state) = s.frames.sh
    let internal pathConditionOf (s : state) = s.pc

    let internal withHeap (s : state) h' = { s with heap = h' }
    let internal withStatics (s : state) m' = { s with statics = m' }

    let internal stackLocationToReference state location =
        StackRef (metadataOfStackLocation state location) location []
    let internal staticLocationToReference term =
        match term.term with
        | Concrete(location, String) -> StaticRef term.metadata (location :?> string) []
        | _ -> __notImplemented__()

    let private heapKeyToString = term >> function
        | Concrete(:? (int list) as k, _) -> k |> List.map toString |> join "."
        | t -> toString t

    let private staticKeyToString = term >> function
        | Concrete(typeName, String) -> System.Type.GetType(typeName :?> string).FullName
        | t -> toString t

    let internal mkMetadata location state =
        { origins = [{ location = location; stack = framesHashOf state}]; misc = null }

// ------------------------------- Memory layer -------------------------------

    type IActivator =
        abstract member CreateInstance : TermMetadata -> System.Type -> Term list -> state -> (Term * state)
    type private NullActivator() =
        interface IActivator with
            member x.CreateInstance _ _ _ _ =
                internalfail "activator is not ready"
    let mutable activator : IActivator = new NullActivator() :> IActivator
    let mutable genericLazyInstantiator : TermMetadata -> Timestamp -> Term -> TermType -> unit -> Term =
        fun _ _ _ _ () -> internalfailf "generic lazy instantiator is not ready"

    let internal stackLazyInstantiator state time key =
        let time = frameTime state key in
        let t = typeOfStackLocation state key in
        let metadata = metadataOfStackLocation state key in
        let fql = StackRef metadata key [] in
        (genericLazyInstantiator metadata time fql t (), time, time)

    let private compositionToString s1 s2 =
        sprintf "%s o %s" s1 s2

    let rec private dumpGeneralizedHeap keyToString n (concrete : System.Text.StringBuilder) = function
        | Defined s when Heap.isEmpty s -> "<empty>", n, concrete
        | Defined s ->
            let freshIdentifier = sprintf "s%d" n in
            freshIdentifier, n+1, concrete.AppendLine(sprintf "\n---------- %s= ----------" freshIdentifier).Append(Heap.dump s keyToString)
        | HigherOrderApplication(f, _, _) -> sprintf "app(%O)" f, n, concrete
        | RecursiveApplication(f, _, _) -> sprintf "recapp(%O)" f, n, concrete // TODO: add recursive definition into concrete section
        | Composition(state, h') ->
            let s, n, concrete = dumpMemoryRec state n concrete in
            let s', n, concrete = dumpGeneralizedHeap keyToString n concrete h' in
            compositionToString s s', n, concrete
        | Merged ghs ->
            let gss, (n, concrete) =
                List.mapFold (fun (n, concrete) (g, h) ->
                        let s, n, concrete = dumpGeneralizedHeap keyToString n concrete h in
                        sprintf "(%O, %s)" g s, (n, concrete))
                    (n, concrete) ghs
            in gss |> join ", " |> sprintf "merge[%s]", n, concrete

    and private dumpMemoryRec s n concrete =
        let sh, n, concrete = dumpGeneralizedHeap heapKeyToString n concrete s.heap in
        let mh, n, concrete = dumpGeneralizedHeap staticKeyToString n concrete s.statics in
        (sprintf "{ heap=%s, statics=%s }" sh mh, n, concrete)

    let internal dumpMemory (s : state) =
        let dump, _, concrete = dumpMemoryRec s 0 (new System.Text.StringBuilder()) in
        if concrete.Length = 0 then dump else sprintf "%s where%O" dump concrete

// ------------------------------- Merging -------------------------------

    let private merge2GeneralizedHeaps g1 g2 h1 h2 resolve =
        match h1, h2 with
        | Defined h1, Defined h2 -> Heap.merge2 h1 h2 resolve |> Defined
        | _ -> Merged [(g1, h1); (g2, h2)]

    let internal mergeGeneralizedHeaps guards heaps resolve mergeSame =
        let defined, undefined =
            heaps
                |> List.zip guards
                |> List.mappedPartition (function | (g, Defined s) -> Some(g, s) | _ -> None)
        in
        let definedGuards, definedHeaps = List.unzip defined in
        let definedHeap = Heap.merge definedGuards definedHeaps resolve |> Defined in
        if undefined.IsEmpty then definedHeap
        else
            let defined = definedGuards |> List.map (withSnd definedHeap) in
            List.append defined undefined |> mergeSame |> Merged

    let internal merge2 g1 g2 (s1 : state) (s2 : state) resolve : state =
        assert(s1.pc = s2.pc)
        assert(s1.frames = s2.frames)
        let mergedStack = MappedStack.merge2 s1.stack s2.stack resolve (stackLazyInstantiator s1) in
        let mergedHeap = merge2GeneralizedHeaps g1 g2 s1.heap s2.heap resolve in
        let mergedStatics = merge2GeneralizedHeaps g1 g2 s1.statics s2.statics resolve in
        { s1 with stack = mergedStack; heap = mergedHeap; statics = mergedStatics }

    let internal merge guards states resolve mergeSame : state =
        assert(List.length states > 0)
        let first = List.head states in
        let frames = framesOf first in
        let path = pathConditionOf first in
        assert(List.forall (fun s -> framesOf s = frames) states)
        assert(List.forall (fun s -> pathConditionOf s = path) states)
        let mergedStack = MappedStack.merge guards (List.map stackOf states) resolve (stackLazyInstantiator first) in
        let mergedHeap = mergeGeneralizedHeaps guards (List.map heapOf states) resolve mergeSame in
        let mergedStatics = mergeGeneralizedHeaps guards (List.map staticsOf states) resolve mergeSame in
        { stack = mergedStack; heap = mergedHeap; statics = mergedStatics; frames = frames; pc = path }
