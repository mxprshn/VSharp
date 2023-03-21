namespace VSharp.Interpreter.IL

open VSharp.Core
open VSharp

type sequenceSearcherResult =
    | Exists of methodSequence
    | NotExist
    | Unknown

type IMethodSequenceSearcher =
    abstract member Pick : unit -> cilState option
    abstract member Update : cilState -> cilState seq -> unit
    abstract member Finished : cilState -> unit
    abstract member FinishedWithError : cilState -> unit

    abstract member TryGetSequence : cilState -> sequenceSearcherResult

// Ничего не делать, если все цепочки сгенерены, а ветвлений нет
// TryGetSequence в Forward
// Абстрактный серчер, интерливинг
// Как делать eval (как мапиться)?
type MethodSequenceSearcher(targetMethods : Method seq) =

    interface IMethodSequenceSearcher with
        override x.Pick() = None
        override x.Update parent children = ()
        override x.TryGetSequence(cilState) =
            match cilState.state.methodSequence with
            | Some s -> Exists s
            | None -> Unknown

        override x.FinishedWithError(_) = ()
        override x.Finished(_) = ()
