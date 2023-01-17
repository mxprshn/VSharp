using System;
using System.Collections.Generic;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.Serialization;
using System.Text;
using NUnit.Framework;

namespace VSharp.Test;

[TestFixture]
public class StackTyperTests
{
    private static void GenerateTest1(ILGenerator il)
    {
        var brLabel = il.DefineLabel();
        var retLabel = il.DefineLabel();

        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Brfalse_S, brLabel);

        il.Emit(OpCodes.Ldstr, "foo");

        il.Emit(OpCodes.Br_S, retLabel);

        il.MarkLabel(brLabel);
        il.Emit(OpCodes.Ldstr, "bar");

        il.MarkLabel(retLabel);
        il.Emit(OpCodes.Callvirt, typeof(string).GetMethod("get_Length"));
        il.Emit(OpCodes.Ret);
    }

    private static void GenerateTest2(ILGenerator il)
    {
        var brLabel = il.DefineLabel();
        var retLabel = il.DefineLabel();

        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Brfalse_S, brLabel);

        il.Emit(OpCodes.Ldstr, "foo");

        il.Emit(OpCodes.Br_S, retLabel);

        il.MarkLabel(brLabel);
        //il.Emit(OpCodes.Ldstr, "bar");
        //Type[] args = { typeof(string) };
        il.Emit(OpCodes.Newobj, typeof(List<int>).GetConstructor(Type.EmptyTypes));

        il.MarkLabel(retLabel);
        il.Emit(OpCodes.Call, typeof(string).GetMethod("get_Length"));
        il.Emit(OpCodes.Ret);
    }

    private static void GenerateTest3(ILGenerator il)
    {
        var brLabel = il.DefineLabel();
        var retLabel = il.DefineLabel();

        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Brfalse_S, brLabel);

        il.Emit(OpCodes.Ldstr, "foo");

        il.Emit(OpCodes.Br_S, retLabel);

        il.MarkLabel(brLabel);
        il.Emit(OpCodes.Ldc_I4_0);

        il.MarkLabel(retLabel);
        il.Emit(OpCodes.Call, typeof(string).GetMethod("get_Length"));
        il.Emit(OpCodes.Ret);
    }

    [Test]
    public static void ClosestCommonSupertypeTest()
    {
        var va = EvaluationStackTyper.closestCommonSupertype(typeof(MethodBuilder), typeof(MethodBase)).Value;
        Console.WriteLine($"{va}");
        Assert.Pass();
    }

    [Test]
    public static void Test1()
    {
        var dynamicAssemblyName = "Test";
        var assemblyBuilder =
            AssemblyBuilder.DefineDynamicAssembly(new AssemblyName(dynamicAssemblyName), AssemblyBuilderAccess.Run);
        var moduleBuilder = assemblyBuilder.DefineDynamicModule(dynamicAssemblyName);

        var typeBuilder = moduleBuilder.DefineType("TestType", TypeAttributes.Public);
        var methodBuilder = typeBuilder.DefineMethod("TestMethod", MethodAttributes.Public | MethodAttributes.Static);
        methodBuilder.SetReturnType(typeof(int));

        Type[] args = { typeof(bool) };
        methodBuilder.SetParameters(args);

        GenerateTest3(methodBuilder.GetILGenerator());

        var typ = typeBuilder.CreateType();

        var m = typ.GetMethod("TestMethod");

        var body = m.GetMethodBody();

        var ilBytes = body.GetILAsByteArray();
        var methodModule = m.Module;
        var moduleName = methodModule.FullyQualifiedName;
        var assemblyName = methodModule.Assembly.FullName;
        var ehs = global::System.Array.Empty<rawExceptionHandler>();
        var props = new rawMethodProperties(
                (uint)m.MetadataToken,
                (uint)ilBytes.Length,
                0u,
                0u,
                (uint)body.MaxStackSize,
                0u
            );
        var tokens = (signatureTokens)FormatterServices.GetUninitializedObject(typeof(signatureTokens));
        var rawBody = new rawMethodBody(
                props,
                assemblyName,
                moduleName,
                tokens,
                ilBytes,
                ehs
            );
        var kek = new ILRewriter(rawBody);
        kek.Import();

        var instrs = kek.CopyInstructions();

        var lol = "";//m.Invoke(null, new object[1] { false });

        Console.WriteLine($"{instrs.Length} {lol}");
    }
}
