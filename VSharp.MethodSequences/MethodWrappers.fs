namespace VSharp.MethodSequences

open System.Reflection
open System.Reflection.Emit
open VSharp
open VSharp.Core

module MethodWrappers =
    
    let private ctorWrapperName = "CtorCall"
    let private wrappersAssemblyName = "MethodSequenceWrappers"

    let private wrappersModuleBuilder =
        let wrappersAssemblyName = wrappersAssemblyName
        let assemblyBuilder = AssemblyManager.DefineDynamicAssembly(AssemblyName wrappersAssemblyName, AssemblyBuilderAccess.Run)
        assemblyBuilder.DefineDynamicModule wrappersAssemblyName
        
    let isConstructorWrapper (method : IMethod) : bool =
        method.Name = ctorWrapperName && method.DeclaringType.Assembly.GetName().Name = wrappersAssemblyName
        
    let getConstructorWrapper (ctor : IMethod): IMethod =
        assert ctor.IsConstructor
        // TODO: get rid of MethodBase access
        let wrapperTypeName = $"CtorWrapper-{ctor.DeclaringType.MetadataToken}-{ctor.MethodBase.MetadataToken}"
        let wrapperMethodName = ctorWrapperName
        let existingType = wrappersModuleBuilder.GetType(wrapperTypeName, false, false)
        let typ =
            if existingType <> null then existingType
            else
                let typeBuilder = wrappersModuleBuilder.DefineType wrapperTypeName
                let (|||) = Microsoft.FSharp.Core.Operators.(|||)
                let methodBuilder = typeBuilder.DefineMethod(wrapperMethodName, MethodAttributes.Public ||| MethodAttributes.Static)
                let parameterTypes = ctor.Parameters |> Array.map (_.ParameterType)
                methodBuilder.SetParameters parameterTypes
                let returnType = ctor.DeclaringType
                methodBuilder.SetReturnType returnType
                for paramIndex in 1..parameterTypes.Length do
                    methodBuilder.DefineParameter(paramIndex, ParameterAttributes.None, $"param_{paramIndex}") |> ignore
                let generator = methodBuilder.GetILGenerator()
                for i in 0..(parameterTypes.Length - 1) do
                    generator.Emit(OpCodes.Ldarg, i)
                let ctorInfo = ctor.MethodBase :?> ConstructorInfo
                generator.Emit(OpCodes.Newobj, ctorInfo)
                generator.Emit OpCodes.Ret
                typeBuilder.CreateType()
        let wrapperMethod = typ.GetMethod wrapperMethodName |> Application.getMethod
        wrapperMethod :> IMethod
