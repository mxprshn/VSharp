namespace VSharp.Core

type methodSequence =
    {
        toDo : unit
    }

    // По сути -- модель sequence-а
    // Просто прокинуть не сможем, можно сделать частичный eval
    member x.Eval (term : term) = True
