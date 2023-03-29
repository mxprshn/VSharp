namespace VSharp.Interpreter.IL

open System
open System.Collections.Generic
open FSharpx.Collections
open VSharp.Core
open VSharp
open VSharp.Core.API

type sequenceSearcherResult =
    | Exists of methodSequence
    | NotExist
    | Unknown

type IMethodSequenceSearcher =
    abstract member Pick : unit -> methodSequenceState option
    abstract member Update : methodSequenceState -> methodSequenceState seq -> unit
    abstract member TryGetSequence : cilState -> sequenceSearcherResult

// Ничего не делать, если все цепочки сгенерены, а ветвлений нет
// TryGetSequence в Forward
// Абстрактный серчер, интерливинг
// Как делать eval (как мапиться)?
type MethodSequenceSearcher(targetMethods : Method seq) =

    let states = System.Collections.Generic.Queue<methodSequenceState>()
    let finished = HashSet<methodSequenceState>()

    do
        let method = Seq.head targetMethods
        if method.IsPublic then
        // TODO: cope with multiple target methods
            method |> MethodSequenceState.createInitial |> states.Enqueue

    let pick() =
        if states.Count = 0 then
            None
        else
            states.Dequeue() |> Some

    let update (children : methodSequenceState seq) =
        for child in children do
            if MethodSequenceState.isFinished child then
                finished.Add child |> ignore
            else
                states.Enqueue child

    interface IMethodSequenceSearcher with
        override x.Pick() = pick()
        override x.Update _ children = update children

        override x.TryGetSequence(cilState) =
            match cilState.state.methodSequence with
            | Some s -> Exists s
            | None ->
                // ... look in cache first
                if states.Count = 0 then
                    NotExist
                else
                    Unknown
