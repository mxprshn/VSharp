namespace VSharp.MethodSequences

open System
open System.Collections.Generic
open VSharp
open VSharp.Core

type varSubstSource =
    | RetVal
    | StackVal of stackKey

    member x.MatchesKey (key : stackKey) : bool =
        match x with
        | StackVal actualKey when actualKey = key -> true
        | _ -> false

type varSubst = pdict<variableId, varSubstSource>

type stackToVar = pdict<stackKey, variableId>

module Substitutions =

    let mapThisAndParametersToVars (method : IMethod) (subst : varSubst) : stackToVar =
        seq {
            if Utils.hasInstanceThis method then
                let thisKey = ThisKey method
                let typ =
                    match PersistentDict.toSeq subst |> Seq.tryFind (fun (_, src) -> src.MatchesKey thisKey) with
                    | Some(id, _) ->
                        id.typ
                    | _ -> thisKey.TypeOfLocation
                yield (thisKey, Variables.getFresh typ)
            for parameterInfo in method.Parameters do
                let parameterKey = ParameterKey parameterInfo
                let typ =
                    match PersistentDict.toSeq subst |> Seq.tryFind (fun (_, src) -> src.MatchesKey parameterKey) with
                    | Some(id, _) ->
                        id.typ
                    | _ -> parameterKey.TypeOfLocation
                yield (parameterKey, Variables.getFresh typ)
        } |> PersistentDict.ofSeq

    let addVars (stackToVar : stackToVar) (variables : variableId list): stackToVar =
        let addVar dict variable =
            let stackKey = Variables.stackKeyOf variable
            PersistentDict.add stackKey variable dict
        Seq.fold addVar stackToVar variables

    let createMappingState (method : IMethod) (stackToVar : stackToVar) (existingVars : variableId list) : state =
        let mappingState = Memory.EmptyIsolatedState()
        Memory.NewStackFrame mappingState None []
        let methodFrame = List<stackKey * term option * Type>()

        let addVar (key : stackKey) (var : variableId) =
            Memory.AllocateTemporaryLocalVariableOfType mappingState (key.ToString()) var.index var.typ |> ignore
            let thisVarTerm = Variables.stackKeyOf var |> Memory.ReadLocalVariable mappingState
            methodFrame.Add(key, Some thisVarTerm, TypeOf thisVarTerm)

        for key in Utils.getThisAndParametersKeys method do
            let var = PersistentDict.find stackToVar key
            addVar key var

        for existingVar in existingVars do
            let key = Variables.stackKeyOf existingVar
            addVar key existingVar

        Memory.NewStackFrame mappingState None (List.ofSeq methodFrame)
        mappingState
