namespace VSharp.MethodSequences

open System
open System.Collections.Generic
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL.CilState

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

    let getStackKeyTypeFromModel (key : stackKey) (cilState : cilState) =
        match cilState.state.model with
        | StateModel modelState ->
            let modelAddress = modelState.memory.ReadStackLocation key
            match modelAddress.term with
            | HeapRef({term = ConcreteHeapAddress(addr)}, _) ->
                match PersistentDict.tryFind modelState.memory.AllocatedTypes addr with
                | Some(ConcreteType concreteTyp) ->
                    concreteTyp
                | _ ->
                    key.TypeOfLocation
            | _ -> key.TypeOfLocation
        | _ -> internalfailf "Wrong model type in target state"

    let mapThisAndParametersToVars (method : IMethod) (cilState : cilState option) (subst : varSubst) : stackToVar =
        seq {
            if Utils.hasInstanceThis method then
                let thisKey = ThisKey method
                let typ =
                    match PersistentDict.toSeq subst |> Seq.tryFind (fun (_, src) -> src.MatchesKey thisKey) with
                    | Some(id, _) ->
                        id.typ
                    | _ ->
                        match cilState with
                        | Some cilState -> getStackKeyTypeFromModel thisKey cilState
                        | _ -> thisKey.TypeOfLocation
                yield (thisKey, Variables.getFresh typ)
            for parameterInfo in method.Parameters do
                let parameterKey = ParameterKey parameterInfo
                let typ =
                    match PersistentDict.toSeq subst |> Seq.tryFind (fun (_, src) -> src.MatchesKey parameterKey) with
                    | Some(id, _) ->
                        id.typ
                    | _ ->
                        match cilState with
                        | Some cilState -> getStackKeyTypeFromModel parameterKey cilState
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
