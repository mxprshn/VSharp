namespace VSharp.IL

open System.Reflection
open VSharp

type StFldAnalyzer() =
    let storedFields =

    member x.Analyze(method : MethodBase, instr : ilInstr) =
        match instr.opcode with
        | OpCode opCode ->
            let opCode = LanguagePrimitives.EnumOfValue opCode.Value
            match opCode with
            | OpCodeValues.Stfld ->
                let fieldInfo = Reflection.resolveField method instr.Arg32
                fieldInfo.
            ()
        | SwitchArg -> ()
