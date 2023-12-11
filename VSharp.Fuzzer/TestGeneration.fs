namespace VSharp.Fuzzer

open System.Reflection
open FSharpx.Collections
open VSharp
open VSharp.Core
open VSharp.Fuzzer
open VSharp.Interpreter.IL

type internal InvocationResult =
    | Thrown of exn
    | Returned of obj

module internal TestGeneration =

    let fuzzingResultToTest generationData invocationResult =

        Logger.traceTestGeneration "Creating state"
        let m = generationData.method
        let state = Memory.EmptyState()
        state.model <- Memory.EmptyModel m
        state.typeStorage <- generationData.typeStorage

        let model =
            match state.model with
            | StateModel(state, _) -> state
            | _ -> internalfail "Unexpected state.model"

        let allocateRef (pi: ParameterInfo) obj =
            let typ = pi.ParameterType.GetElementType()
            let position = pi.Position + 1
            let allocatedObject = Memory.ObjectToTerm state obj typ
            Memory.AllocateTemporaryLocalVariable model position typ allocatedObject

        let allocatePointer (pi: ParameterInfo) obj =
            assert pi.ParameterType.IsPointer
            let pointer = Pointer.Unbox obj
            let realObj = System.Runtime.CompilerServices.Unsafe.Read pointer

            let realObjType = pi.ParameterType.GetElementType()
            let heapAddress =
                match (Memory.AllocateConcreteObject state realObj realObjType).term with
                | HeapRef (address, _ ) -> address
                | _ -> __unreachable__ ()

            let heapLocation = HeapLocation (heapAddress, realObjType)
            let ptr = Ptr heapLocation realObjType (MakeNumber 0)
            ptr

        let (|Mock|Ref|Pointer|Obj|) arg =
            if generationData.instantiatedMocks.ContainsKey arg then
                Mock generationData.instantiatedMocks[arg]
            elif generationData.referencedObjects.Contains arg then
                Ref
            elif generationData.allocatedObjects.Contains arg then
                Pointer
            else
                Obj

        Logger.traceTestGeneration "Creating first frame and filling stack"
        let this =
            if m.HasThis then
                match generationData.this with
                | Obj -> Some (Memory.ObjectToTerm state generationData.this generationData.thisType)
                | Mock mock -> Some (Memory.AllocateMock state mock generationData.thisType)
                | _ -> failwith "Unexpected this type kind: Pointer or Ref"
            else None

        let createTerm (arg, pi: ParameterInfo) =
            let argType = pi.ParameterType
            let result =
                match arg with
                | Mock mock ->
                    Logger.traceTestGeneration $"Allocate mock for parameter: {pi.Position}"
                    Memory.AllocateMock state mock argType
                | Ref ->
                    Logger.traceTestGeneration $"Allocate ref for parameter: {pi.Position}"
                    allocateRef pi arg
                | Pointer ->
                    Logger.traceTestGeneration $"Allocate pointer for parameter: {pi.Position}"
                    allocatePointer pi arg
                | Obj ->
                    Logger.traceTestGeneration $"Create object for parameter: {pi.Position}"
                    Memory.ObjectToTerm state arg argType
            Some result

        if generationData.method.HasParameterOnStack then
            Logger.traceTestGeneration "Method has parameter on stack, create new stack frame"
            Memory.NewStackFrame state None []

        let parameters =
            Array.zip generationData.args generationData.method.Parameters
            |> Array.map createTerm
            |> List.ofArray
        Memory.InitFunctionFrame state m this (Some parameters)
        Memory.InitFunctionFrame model m this (Some parameters)

        // Filling invocation result and create testSuite
        let testSuite =
            match invocationResult with
            | Thrown ex ->
                Logger.traceTestGeneration "Filling exception register"
                let exType = ex.GetType()
                let exRef = Memory.AllocateConcreteObject state ex exType
                // TODO: check if exception was thrown by user or by runtime
                state.exceptionsRegister <- exceptionRegisterStack.singleton <| Unhandled(exRef, false, "")
                Error ("", false)
            | Returned obj ->
                Logger.traceTestGeneration "Pushing result onto evaluation stack"
                let returnedTerm = Memory.ObjectToTerm state obj m.ReturnType
                state.evaluationStack <- EvaluationStack.Push returnedTerm state.evaluationStack
                Test

        Logger.traceTestGeneration "State to test started"
        // Create test from filled state
        let result = TestGenerator.state2testWithMockingCache testSuite m state generationData.mocks
        Logger.traceTestGeneration "State to test finished"
        result
