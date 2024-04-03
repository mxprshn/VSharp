namespace VSharp.MethodSequences

open System
open System.Collections.Generic
open VSharp
open VSharp.Core

type varSubstSource =
    | RetVal
    | StackVal of stackKey
    
type varSubst = pdict<variableId, varSubstSource>

type stackToVar = pdict<stackKey, variableId>

module Substitutions =
    
    let mapThisAndParametersToVars (method : IMethod): stackToVar =
        seq {
            if Utils.hasInstanceThis method then
                let thisKey = ThisKey method
                thisKey, Variables.getFresh thisKey.TypeOfLocation
            for parameterInfo in method.Parameters do
                let parameterKey = ParameterKey parameterInfo
                parameterKey, Variables.getFresh parameterKey.TypeOfLocation
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
