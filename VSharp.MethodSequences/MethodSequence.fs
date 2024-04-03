namespace VSharp.MethodSequences

open System
open VSharp
open VSharp.Core

type methodSequenceElement =
    | Call of IMethod * variableId option * variableId option * variableId list

    override x.ToString() =
        match x with
        | Call(method, retVar, thisVar, parameterVars) ->
            let joinedParams = parameterVars |> Seq.map (_.ToString()) |> join ", "
            match retVar, thisVar with
            | Some(retVar), Some(thisVar) ->
                $"{retVar} = {thisVar}.{method.Name}({joinedParams})"
            | None, None -> $"{method.Name}({joinedParams})"
            | None, Some thisVar -> $"{thisVar}.{method.Name}({joinedParams})"
            | Some(retVar), None -> $"{retVar} = {method.Name}({joinedParams})"
    
type methodSequence =
    { elements : methodSequenceElement list }

    override x.ToString() =
        x.elements |> Seq.map (_.ToString()) |> join "\n"
    
module MethodSequence =
    
    let private getThisId (method : IMethod) (stackToVar : stackToVar) =
        if Utils.hasInstanceThis method then
            let key = ThisKey method
            PersistentDict.find stackToVar key |> Some
        else
            None
            
    let private getParametersIds (method : IMethod) (stackToVar : stackToVar) =
        seq {
            for parameterInfo in method.Parameters do
                let key = ParameterKey parameterInfo
                PersistentDict.find stackToVar key
        } |> Seq.toList
    
    let createInitial (method : IMethod) (stackToVar : stackToVar) : methodSequence =
        let returnValId = None
        let thisId = getThisId method stackToVar
        let parametersIds = getParametersIds method stackToVar
        { elements = [ Call(method, returnValId, thisId, parametersIds) ] }
        
    let addCall (methodSequence : methodSequence) (method : IMethod) (methodStackToVar : stackToVar) (subst : varSubst) : methodSequence =
        let getNewMethodRetValId() =
            let retType = method.ReturnType
            if retType = typeof<Void> then
                None
            else
                Variables.getFresh retType |> Some
        let newMethodRetValId = lazy getNewMethodRetValId()
        
        let mapVar (variableId : variableId) =
            match PersistentDict.tryFind subst variableId with
            | Some substSource ->
                match substSource with
                | RetVal ->
                    match newMethodRetValId.Value with
                    | Some var -> var
                    | None -> __unreachable__()
                | StackVal key -> PersistentDict.find methodStackToVar key
            | None -> variableId
        
        let elements =
            seq {
                let thisId = getThisId method methodStackToVar
                let parametersIds = getParametersIds method methodStackToVar
                yield Call(method, newMethodRetValId.Value, thisId, parametersIds)
                
                for element in methodSequence.elements do
                    match element with
                    | Call(elementMethod, retValId, thisId, parameterIds) ->
                        let mappedThisId = Option.map mapVar thisId
                        let mappedParametersIds = List.map mapVar parameterIds
                        yield Call(elementMethod, retValId, mappedThisId, mappedParametersIds)
            } |> Seq.toList
            
        { elements = elements }
