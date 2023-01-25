using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.Serialization;
using System.Text;
using NUnit.Framework;

namespace VSharp.Test;

[TestFixture]
public class StackTyperTests
{
    private static void SmokeTest(ILGenerator il)
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

    private static stackType[][] _smokeTestStates =
    {
        new stackType[] { },
        new stackType[] { stackType.Int32Typ },
        new stackType[] { },
        new stackType[] { stackType.NewObjectType(typeof(string)) },
        new stackType[] { },
        new stackType[] { stackType.NewObjectType(typeof(string)) },
        new stackType[] { stackType.Int32Typ }
    };

    private interface IA
    {
        public void Foo();
    }

    private class A1 : IA
    {
        public void Foo() { }
    }

    private class A2 : IA
    {
        public void Foo() { }
    }

    private static void StateMergingTest(ILGenerator il)
    {
        var brLabel = il.DefineLabel();
        var retLabel = il.DefineLabel();

        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Brfalse_S, brLabel);

        il.Emit(OpCodes.Newobj, typeof(A1).GetConstructor(Type.EmptyTypes));

        il.Emit(OpCodes.Br_S, retLabel);

        il.MarkLabel(brLabel);
        il.Emit(OpCodes.Newobj, typeof(A2).GetConstructor(Type.EmptyTypes));

        il.MarkLabel(retLabel);
        il.Emit(OpCodes.Callvirt, typeof(IA).GetMethod("Foo"));
        il.Emit(OpCodes.Ret);
    }

    private static stackType[][] _stateMergingTestStates =
    {
        new stackType[] { },
        new[] { stackType.Int32Typ },
        new stackType[] { },
        new[] { stackType.NewObjectType(typeof(A1)) },
        new stackType[] { },
        new[] { stackType.NewObjectType(typeof(IA)) },
        new stackType[] { }
    };

    private class B
    {
        public void Foo() { }
    }

    private class C : B { }

    private static void StateMergingTest2(ILGenerator il)
    {
        var brLabel = il.DefineLabel();
        var retLabel = il.DefineLabel();

        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Brfalse_S, brLabel);

        il.Emit(OpCodes.Newobj, typeof(B).GetConstructor(Type.EmptyTypes));

        il.Emit(OpCodes.Br_S, retLabel);

        il.MarkLabel(brLabel);
        il.Emit(OpCodes.Newobj, typeof(C).GetConstructor(Type.EmptyTypes));

        il.MarkLabel(retLabel);
        il.Emit(OpCodes.Callvirt, typeof(B).GetMethod("Foo"));
        il.Emit(OpCodes.Ret);
    }

    private static stackType[][] _stateMergingTest2States =
    {
        new stackType[] { },
        new stackType[] { stackType.Int32Typ },
        new stackType[] { },
        new stackType[] { stackType.NewObjectType(typeof(B)) },
        new stackType[] { },
        new stackType[] { stackType.NewObjectType(typeof(B)) },
        new stackType[] { }
    };

    private static void StateMergingTestArrays(ILGenerator il)
    {
        var brLabel = il.DefineLabel();
        var retLabel = il.DefineLabel();

        // Array size
        il.Emit(OpCodes.Ldc_I4_1);

        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Brfalse_S, brLabel);

        il.Emit(OpCodes.Newarr, typeof(C));

        il.Emit(OpCodes.Br_S, retLabel);

        il.MarkLabel(brLabel);
        il.Emit(OpCodes.Newarr, typeof(B));

        il.MarkLabel(retLabel);

        // Index
        il.Emit(OpCodes.Ldc_I4_0);
        il.Emit(OpCodes.Ldelem_Ref);
        il.Emit(OpCodes.Ret);
    }

    private static stackType[][] _stateMergingTestArraysStates =
    {
        new stackType[] { },
        new[] { stackType.Int32Typ },
        new[] { stackType.Int32Typ, stackType.Int32Typ },

        new[] { stackType.Int32Typ },
        new[] { stackType.NewObjectType(typeof(C).MakeArrayType()) },

        new[] { stackType.Int32Typ },
        new[] { stackType.NewObjectType(typeof(B).MakeArrayType()) },

        new[] { stackType.Int32Typ, stackType.NewObjectType(typeof(B).MakeArrayType()) },

        new[] { stackType.NewObjectType(typeof(B)) }
    };

    private static void StateMergingTestArrays2(ILGenerator il)
    {
        var brLabel = il.DefineLabel();
        var retLabel = il.DefineLabel();

        // Array size
        il.Emit(OpCodes.Ldc_I4_1);

        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Brfalse_S, brLabel);

        il.Emit(OpCodes.Newarr, typeof(OverflowException));

        il.Emit(OpCodes.Br_S, retLabel);

        il.MarkLabel(brLabel);
        il.Emit(OpCodes.Newarr, typeof(OverflowException));

        il.MarkLabel(retLabel);

        // Index
        il.Emit(OpCodes.Ldc_I4_0);
        il.Emit(OpCodes.Ldelem_Ref);
        il.Emit(OpCodes.Ret);
    }

    private static stackType[][] _stateMergingTestArraysStates2 =
    {
        new stackType[] { },
        new[] { stackType.Int32Typ },
        new[] { stackType.Int32Typ, stackType.Int32Typ },

        new[] { stackType.Int32Typ },
        new[] { stackType.NewObjectType(typeof(OverflowException).MakeArrayType()) },

        new[] { stackType.Int32Typ },
        new[] { stackType.NewObjectType(typeof(OverflowException).MakeArrayType()) },

        new[] { stackType.Int32Typ, stackType.NewObjectType(typeof(OverflowException).MakeArrayType()) },

        new[] { stackType.NewObjectType(typeof(OverflowException)) }
    };

    private static void ArrayListCovariance(ILGenerator il)
    {
        var brLabel = il.DefineLabel();
        var retLabel = il.DefineLabel();

        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Brfalse_S, brLabel);

        // Array size
        il.Emit(OpCodes.Ldc_I4_1);
        il.Emit(OpCodes.Newarr, typeof(int));

        il.Emit(OpCodes.Br_S, retLabel);

        il.MarkLabel(brLabel);
        il.Emit(OpCodes.Newobj, typeof(List<int>).GetConstructor(Type.EmptyTypes));

        il.MarkLabel(retLabel);

        il.Emit(OpCodes.Ret);
    }

    private static stackType[][] _arrayListCovarianceStates =
    {
        new stackType[] { },
        new[] { stackType.Int32Typ },

        new stackType[] { },
        new stackType[] { stackType.Int32Typ },
        new[] { stackType.NewObjectType(typeof(int).MakeArrayType()) },

        new stackType[] { },

        new[] { stackType.NewObjectType(typeof(IList<int>)) }
    };

    // IL of the following method:
    //
    /*public static int DecimalTest(decimal sum)
    {
        return sum switch
        {
            <= 1_000_000 => 12,
            <= 5_000_000 => 14,
            <= 10_000_000 => 8,
            _ => throw new ArgumentOutOfRangeException()
        };
    }*/
    private static void Decimals(ILGenerator il)
    {
        var labelBranch = il.DefineLabel();
        var label12 = il.DefineLabel();
        var label14 = il.DefineLabel();
        var label8 = il.DefineLabel();
        var labelThrow = il.DefineLabel();
        var labelRet = il.DefineLabel();

        il.DeclareLocal(typeof(int));

        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Ldc_I4, 5000000);
        il.Emit(OpCodes.Newobj, typeof(decimal).GetConstructor(new[] { typeof(int) }));
        il.Emit(OpCodes.Call, typeof(decimal).GetMethod("op_LessThanOrEqual"));
        il.Emit(OpCodes.Brfalse_S, labelBranch);

        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Ldc_I4, 1000000);
        il.Emit(OpCodes.Newobj, typeof(decimal).GetConstructor(new[] { typeof(int) }));
        il.Emit(OpCodes.Call, typeof(decimal).GetMethod("op_LessThanOrEqual"));
        il.Emit(OpCodes.Brtrue_S, label12);
        il.Emit(OpCodes.Br_S, label14);

        il.MarkLabel(labelBranch);
        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Ldc_I4, 10000000);
        il.Emit(OpCodes.Newobj, typeof(decimal).GetConstructor(new[] { typeof(int) }));
        il.Emit(OpCodes.Call, typeof(decimal).GetMethod("op_LessThanOrEqual"));
        il.Emit(OpCodes.Brtrue_S, label8);
        il.Emit(OpCodes.Br_S, labelThrow);

        il.MarkLabel(label12);
        il.Emit(OpCodes.Ldc_I4_S, (byte)12);

        il.Emit(OpCodes.Stloc_0);
        il.Emit(OpCodes.Br_S, labelRet);

        il.MarkLabel(label14);
        il.Emit(OpCodes.Ldc_I4_S, (byte)14);
        il.Emit(OpCodes.Stloc_0);
        il.Emit(OpCodes.Br_S, labelRet);

        il.MarkLabel(label8);
        il.Emit(OpCodes.Ldc_I4_8);
        il.Emit(OpCodes.Stloc_0);
        il.Emit(OpCodes.Br_S, labelRet);

        il.MarkLabel(labelThrow);
        il.Emit(OpCodes.Newobj, typeof(ArgumentOutOfRangeException).GetConstructor(Type.EmptyTypes));
        il.Emit(OpCodes.Throw);

        il.MarkLabel(labelRet);
        il.Emit(OpCodes.Ldloc_0);
        il.Emit(OpCodes.Ret);
    }

    private static stackType[][] _decimalsStates =
    {
        new stackType[] { },
        new [] { stackType.NewValueType(typeof(decimal)) },
        new [] { stackType.Int32Typ, stackType.NewValueType(typeof(decimal)) },
        new [] { stackType.NewValueType(typeof(decimal)), stackType.NewValueType(typeof(decimal)) },
        new [] { stackType.Int8Type },

        new stackType[] { },
        new [] { stackType.NewValueType(typeof(decimal)) },
        new [] { stackType.Int32Typ, stackType.NewValueType(typeof(decimal)) },
        new [] { stackType.NewValueType(typeof(decimal)), stackType.NewValueType(typeof(decimal)) },
        new [] { stackType.Int8Type },
        new stackType[] { },

        new stackType[] { },
        new [] { stackType.NewValueType(typeof(decimal)) },
        new [] { stackType.Int32Typ, stackType.NewValueType(typeof(decimal)) },
        new [] { stackType.NewValueType(typeof(decimal)), stackType.NewValueType(typeof(decimal)) },
        new [] { stackType.Int8Type },
        new stackType[] { },

        new stackType[] { },
        new[] { stackType.Int32Typ },
        new stackType[] { },

        new stackType[] { },
        new[] { stackType.Int32Typ },
        new stackType[] { },

        new stackType[] { },
        new[] { stackType.Int32Typ },
        new stackType[] { },

        new stackType[] { },
        new[] { stackType.NewObjectType(typeof(ArgumentOutOfRangeException)) },

        new stackType[] { },
        new[] { stackType.Int32Typ }
    };

    private static object[] _stackTyperTestCases =
    {
        new object[] { Decimals, new[] { typeof(decimal) }, typeof(int), _decimalsStates },
        new object[] { SmokeTest, new[] { typeof(int) }, typeof(int), _smokeTestStates },
        new object[] { StateMergingTest, new[] { typeof(int) }, null, _stateMergingTestStates },
        new object[] { StateMergingTest2, new[] { typeof(int) }, null, _stateMergingTest2States },
        new object[] { StateMergingTestArrays, new[] { typeof(int) }, typeof(B), _stateMergingTestArraysStates },
        new object[] { StateMergingTestArrays2, new[] { typeof(int) }, typeof(OverflowException), _stateMergingTestArraysStates2 },
        new object[] { ArrayListCovariance, new[] { typeof(int) }, typeof(B), _arrayListCovarianceStates }
    };

    [Test]
    [TestCaseSource(nameof(_stackTyperTestCases))]
    public static void StackTyperTest(Action<ILGenerator> ilCodeGenerator, Type[] argTypes, Type returnType, stackType[][] expectedStackStates)
    {
        var dynamicAssemblyName = "StackTyperTest";
        var assemblyBuilder =
            AssemblyBuilder.DefineDynamicAssembly(new AssemblyName(dynamicAssemblyName), AssemblyBuilderAccess.Run);
        var moduleBuilder = assemblyBuilder.DefineDynamicModule(dynamicAssemblyName);

        var typeBuilder = moduleBuilder.DefineType("StackTyperTestType", TypeAttributes.Public);
        var methodBuilder = typeBuilder.DefineMethod("StackTyperTestMethod", MethodAttributes.Public | MethodAttributes.Static);

        if (returnType is not null)
        {
            methodBuilder.SetReturnType(returnType);
        }

        methodBuilder.SetParameters(argTypes);
        ilCodeGenerator(methodBuilder.GetILGenerator());

        var typ = typeBuilder.CreateType();
        var m = typ.GetMethod("StackTyperTestMethod");
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
        var rewriter = new ILRewriter(rawBody);
        rewriter.Import();

        var instructions = rewriter.CopyInstructions();

        Assert.AreEqual(expectedStackStates.Length, instructions.Length);

        for (var i = 0; i < instructions.Length; ++i)
        {
            var actualStackState = instructions[i].stackState.Value.ToArray();

            Assert.AreEqual(expectedStackStates[i].Length, actualStackState.Length);

            for (var j = 0; j < actualStackState.Length; ++j)
            {
                Assert.AreEqual(expectedStackStates[i][j], actualStackState[j]);
            }
        }
    }
}
