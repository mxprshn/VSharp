namespace VSharp.MethodSequences

open System
open System.Collections.Generic
open System.Reflection
open VSharp.CSharpUtils
open VSharp.Core
open VSharp.Core.API
open VSharp.Interpreter.IL.CilState

type SupportValidation =

    static let isMockResultConstant (constant : term) =
        match constant.term with
        | Constant(_, :? functionResultConstantSource, _) -> true
        | _ -> false

    static member IsSupported (state : cilState) =
        let constantsInCondition = Terms.GetConstants (PathConditionToSeq state.state.pc)
        Seq.forall (not << isMockResultConstant) constantsInCondition
