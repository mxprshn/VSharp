namespace VSharp.Interpreter.IL

open System
open FSharpx.Collections
open VSharp.Core
open VSharp.Core.API
open VSharp

type IMethodSequenceElementsSource =
    abstract GetStatesToPush : methodSequenceState -> cilState seq

type IMethodSequenceExplorer =
    abstract MakeStep : methodSequenceState -> methodSequenceState seq

type internal MethodSequenceExplorer(interpreter : ILInterpreter, elementsSource : IMethodSequenceElementsSource) =

    let isExitingFromMethod (state : methodSequenceState) =
        let nopMethod = Application.getMethod Loader.MethodSequenceUtilsNop
        match state.cilState.ipStack with
        // Может быть exit из m?
        | Exit _ :: Instruction(_, m) :: _ when m = nopMethod -> true
        | _ -> false

    let exitFromMethod (state : methodSequenceState) =
        let cilState = state.cilState
        let method = cilState.currentLoc.method

        let result =
            // evaluation stack суммируется для всех фреймов
            match EvaluationStack.Length cilState.state.evaluationStack with
            | 0 -> None
            | 1 ->
                let result = EvaluationStack.Pop cilState.state.evaluationStack |> fst
                Types.Cast result method.ReturnType |> Some
            | _ -> __unreachable__()

        let thisAndParameters = seq {
            if method.HasThis then
                yield Memory.ReadThis cilState.state method

            // It is likely to fail on value types!
            for parameter in method.Parameters ->
                Memory.ReadArgument cilState.state parameter
        }

        let exitedState, iieStates, errorStates = interpreter.ExecuteOneInstruction state.cilState
        assert(List.isEmpty iieStates && List.isEmpty errorStates && List.length exitedState = 1)
        let exitedState = List.head exitedState

        let resultVariable, variables =
            match result with
            | None -> None, state.localVariables
            | Some resultTerm ->
                let index, variables =
                    if PersistentHashMap.containsKey method.ReturnType state.localVariables then
                        let currentIndex = PersistentHashMap.find method.ReturnType state.localVariables
                        currentIndex, PersistentHashMap.add method.ReturnType (currentIndex + 1) state.localVariables
                    else
                        0, PersistentHashMap.add method.ReturnType 1 state.localVariables
                // Проверить AllocateTemporaryLocalVariable, не будет ли пересечений
                // Конструктор value типа
                Memory.AllocateTemporaryLocalVariable exitedState.state index method.ReturnType resultTerm |> Some, variables

        let methodsToCall =
            match state.methodsToCall with
            | [] -> __unreachable__()
            | calledMethod :: remainingMethods ->
                assert(calledMethod = method)
                remainingMethods

        let call = Call(resultVariable, method, Seq.toList thisAndParameters)

        // What to do with objects to create?
        {
            state with
                isInMethod = false
                cilState = exitedState
                currentSequence = call :: state.currentSequence
                methodsToCall = methodsToCall
                localVariables = variables
        }

    let goForwardInsideMethod (state : methodSequenceState) =
        // Чекнуть, что с тем стейтом, что приходит
        let goodStates, _, _ = interpreter.ExecuteOneInstruction state.cilState
        seq {
            // Maybe we can avoid copying
            for goodState in goodStates -> { state with cilState = goodState }
        }

    let getPossibleInputs (state : )

    let tryPopAndCallMethod (state : methodSequenceState) : methodSequenceState seq =
        match state.targets with
        | [] -> __unreachable__()
        | [targetBranch] ->
            // subst with all possible values
            let evaled = state.cilState.state.model.Eval targetBranch.state.pc

        | branch :: otherBranches ->


    let pushMethods (state : methodSequenceState) : methodSequenceState seq =
        seq {
            for stateToPush in elementsSource.GetStatesToPush state ->
                { state with targets = stateToPush :: state.targets }
        }

    let makeStep (state : methodSequenceState) =
        seq {
            yield! tryPopAndCallMethod state
            yield! pushMethods state
        }

    interface IMethodSequenceExplorer with
        member this.MakeStep state = makeStep state
