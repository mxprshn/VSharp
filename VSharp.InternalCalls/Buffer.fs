namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilStateOperations

// ------------------------------ System.Buffer --------------------------------

module internal Buffer =

    let private getAddress cilState ref =
        let state = cilState.state
        let cases = Memory.TryAddressFromRefFork state ref
        assert(List.length cases >= 1)
        let createArrayRef (address, state) =
            let cilState = changeState cilState state
            match address with
            | Some address -> Some address, cilState
            | _ ->
                let iie = createInsufficientInformation "Memmove: unknown pointer"
                cilState.iie <- Some iie
                None, cilState
        List.map createArrayRef cases

    let private Copy dstAddr dstIndex dstIndices dstArrayType srcAddr srcIndex srcIndices srcArrayType state bytesCount =
        if Memory.IsSafeContextCopy srcArrayType dstArrayType |> not then
            internalfail $"Buffer.Copy: unsafe memory copy is not implemented, src type {srcArrayType}, dst type {dstArrayType}"
        let size = TypeUtils.internalSizeOf (fst3 srcArrayType)
        let elemCount = Arithmetics.Div bytesCount (MakeNumber size)
        let dstType = Types.ArrayTypeToSymbolicType dstArrayType
        let srcType = Types.ArrayTypeToSymbolicType srcArrayType
        let dstHeapRef = HeapRef dstAddr dstType
        let srcHeapRef = HeapRef srcAddr srcType
        let dstLinearIndex = Memory.LinearizeArrayIndex state dstAddr dstIndices dstArrayType
        let dstLinearIndex =
            match dstIndex with
            | Some dstIndex -> API.Arithmetics.Add dstLinearIndex dstIndex
            | None -> dstLinearIndex
        let srcLinearIndex = Memory.LinearizeArrayIndex state srcAddr srcIndices srcArrayType
        let srcLinearIndex =
            match srcIndex with
            | Some srcIndex -> API.Arithmetics.Add srcLinearIndex srcIndex
            | None -> srcLinearIndex
        Memory.CopyArray state srcHeapRef srcLinearIndex srcType dstHeapRef dstLinearIndex dstType elemCount

    let CommonMemmove (cilState : cilState) dst dstIndex src srcIndex bytesCount =
        if bytesCount = MakeNumber 0 then List.singleton cilState
        else
            let state = cilState.state
            let bytesCount = Types.Cast bytesCount typeof<int>
            let checkDst (info, cilState) =
                match info with
                | Some (dstAddress : address) ->
                    let checkSrc (info, cilState) =
                        match info with
                        | Some (srcAddress : address) ->
                            let dstType = lazy dstAddress.TypeOfLocation
                            let srcType = lazy srcAddress.TypeOfLocation
                            let dstSize = lazy(TypeUtils.internalSizeOf dstType.Value)
                            let srcSize = lazy(TypeUtils.internalSizeOf srcType.Value)
                            let allSafe() =
                                let isSafe =
                                    dstType.Value = srcType.Value
                                    || TypeUtils.canCastImplicitly srcType.Value dstType.Value
                                    && dstSize.Value = srcSize.Value
                                isSafe && MakeNumber srcSize.Value = bytesCount
                            let dstSafe = lazy(MakeNumber dstSize.Value = bytesCount)
                            let srcSafe = lazy(MakeNumber srcSize.Value = bytesCount)
                            match dstAddress, srcAddress with
                            | _ when allSafe() ->
                                let value = read cilState (Ref srcAddress)
                                let cilStates = write cilState (Ref dstAddress) value
                                assert(List.length cilStates = 1)
                                List.head cilStates
                            | _ when dstSafe.Value ->
                                let ptr = Types.Cast (Ref srcAddress) (dstType.Value.MakePointerType())
                                let value = read cilState ptr
                                let cilStates = write cilState (Ref dstAddress) value
                                assert(List.length cilStates = 1)
                                List.head cilStates
                            | _ when srcSafe.Value ->
                                let value = read cilState (Ref srcAddress)
                                let ptr = Types.Cast (Ref dstAddress) (srcType.Value.MakePointerType())
                                let cilStates = write cilState ptr value
                                assert(List.length cilStates = 1)
                                List.head cilStates
                            | ArrayIndex(dstAddress, dstIndices, dstArrayType), ArrayIndex(srcAddress, srcIndices, srcArrayType) ->
                                Copy dstAddress dstIndex dstIndices dstArrayType srcAddress srcIndex srcIndices srcArrayType state bytesCount
                                cilState
                            // TODO: implement unsafe copy
                            | _ -> internalfail $"CommonMemmove unexpected addresses {srcAddress}, {dstAddress}"
                        | None -> cilState
                    getAddress cilState src |> List.map checkSrc
                | None -> cilState |> List.singleton
            getAddress cilState dst |> List.collect checkDst

    let Memmove (cilState : cilState) dst src bytesCount =
        CommonMemmove cilState dst None src None bytesCount

    let GenericMemmove (_ : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 4)
        let elemType, dst, src, elemCount = args[0], args[1], args[2], args[3]
        let elemSize = Helpers.unwrapType elemType |> TypeUtils.internalSizeOf
        let bytesCount = Arithmetics.Mul elemCount (MakeNumber elemSize)
        Memmove cilState dst src bytesCount

    let ByteMemmove (_ : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 3)
        let dst, src, bytesCount = args[0], args[1], args[2]
        Memmove cilState dst src bytesCount

    let MemoryCopy (i : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 4)
        let dst, src, dstBytesCount, bytesCount = args[0], args[1], args[2], args[3]
        let memMove cilState k =
            Memmove cilState dst src bytesCount |> k
        let checkDst cilState k =
            i.NpeOrInvoke cilState dst memMove k
        let checkSrc cilState k =
            i.NpeOrInvoke cilState src checkDst k
        StatedConditionalExecutionCIL cilState
            (fun state k -> k (Arithmetics.LessOrEqual bytesCount dstBytesCount, state))
            checkSrc
            (i.Raise i.ArgumentOutOfRangeException)
            id
