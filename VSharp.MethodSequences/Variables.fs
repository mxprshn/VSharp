namespace VSharp.MethodSequences

open System
open System.Collections.Generic
open VSharp.Core

type variableId =
    { typ : Type; index : int }

    override x.ToString() =
        $"{x.typ.Name}_{x.index}"

module Variables =
    
    let private ids = Dictionary<Type, int>()
    
    let stackKeyOf (varId : variableId) =
        TemporaryLocalVariableKey(varId.typ, varId.index)
        
    let getFresh (typ : Type) =
        let currentIndex = ref 0
        if ids.TryGetValue(typ, currentIndex) then
            let varIndex = currentIndex.Value
            ids[typ] <- varIndex + 1
            { typ = typ; index = varIndex }
        else
            ids[typ] <- 1
            { typ = typ; index = 0 }
