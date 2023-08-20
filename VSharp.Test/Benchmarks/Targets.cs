using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using IntegrationTests;
using NUnit.Framework;

namespace VSharp.Test.Benchmarks;

internal static class Targets
{
    private const BindingFlags AllFlags = BindingFlags.Static | BindingFlags.Instance | BindingFlags.Public | BindingFlags.
        NonPublic;

    private static IEnumerable<Type> GetTypesChecked(this Assembly assembly)
    {
        var toReturn = new List<Type>();

        try
        {
            toReturn.AddRange(assembly.GetTypes());
        }
        catch (ReflectionTypeLoadException e)
        {
            toReturn.AddRange(e.Types.Where(t => t is not null));

            foreach (var loaderException in e.LoaderExceptions.Where(e => e is not null))
            {
                TestContext.Progress.WriteLine(loaderException.Message);
            }
        }

        return toReturn;
    }

    private static bool CheckMethod(Type type, MethodBase method)
    {
        if (method.DeclaringType != type)
        {
            return false;
        }

        try
        {
            if (method.GetMethodBody() is null)
            {
                return false;
            }
        }
        catch (Exception e)
        {
            TestContext.Progress.WriteLine($"Failed to get {method.DeclaringType!.Name}.{method.Name}: {e.Message}");
        }

        return true;
    }

    private static Assembly LoadBenchmarkAssembly(string suite, string dllFileName)
    {
        var dllPath = TestContext.Parameters["BenchmarkDllsPath"];
        if (dllPath is null)
        {
            throw new Exception("Cannot read dll directory path from test context parameters");
        }

        var assemblyPath = Path.Combine(dllPath, suite, $"{dllFileName}.dll");
        return AssemblyManager.LoadFromAssemblyPath(assemblyPath);
    }

    private static BenchmarkTarget GetTargetByToken(this Assembly assembly, int typeToken, int methodToken)
    {
        var type = assembly.Modules.First().ResolveType(typeToken);
        return new(type.GetMethods(AllFlags).First(m => m.MetadataToken == methodToken));
    }

    private static IEnumerable<BenchmarkTarget> GetTargetsForAllMethods(this Assembly assembly, int typeToken)
    {
        var type = assembly.Modules.First().ResolveType(typeToken);
        return type.GetTargetsForAllMethods();
    }

    private static IEnumerable<BenchmarkTarget> GetTargetsForAllMethods(this Type type)
    {
        foreach (var m in type.GetMethods(AllFlags).Where(m => CheckMethod(type, m)))
        {
            yield return new BenchmarkTarget(m);
        }

        foreach (var ctor in type.GetConstructors(AllFlags).Where(ctor => CheckMethod(type, ctor)))
        {
            yield return new BenchmarkTarget(ctor);
        }
    }

    private static IEnumerable<BenchmarkTarget> GetTargetsForAllMethods(this Assembly assembly)
    {
        return assembly.GetTypesChecked().SelectMany(type => type.GetTargetsForAllMethods());
    }

    private static Assembly Lifetimes() => LoadBenchmarkAssembly("jb_lifetimes", "JetBrains.Lifetimes");
    private static Assembly BizHawkCores() => LoadBenchmarkAssembly("bizhawk", "BizHawk.Emulation.Cores");
    private static Assembly UnityCollections() => LoadBenchmarkAssembly("unity_collections", "Unity.Collections");
    private static Assembly UnityHighDefinition() => LoadBenchmarkAssembly("unity_hd", "Unity.RenderPipelines.HighDefinition");

    internal static class BizHawk
    {
        public static BenchmarkTarget TurboCoreDoInit => BizHawkCores().GetTargetByToken(0x200026D, 0x6000149);
        public static BenchmarkTarget LR35902ExecuteOne => BizHawkCores().GetTargetByToken(0x200008A, 0x600091E);
    }

    internal static class Unity
    {
        internal static class Collections
        {
            public static IEnumerable<BenchmarkTarget> All => UnityCollections().GetTargetsForAllMethods();
            public static BenchmarkTarget Hash64Internal => UnityCollections().GetTargetByToken(0x2000089, 0x600085C);
            public static BenchmarkTarget Hash128Internal => UnityCollections().GetTargetByToken(0x2000089, 0x600085C);
        }

        internal static class HighDefinition
        {
            public static IEnumerable<BenchmarkTarget> HlslTree => UnityHighDefinition().GetTargetsForAllMethods(0x200001B);
        }
    }

    internal static class VSharp
    {
        public static BenchmarkTarget LoanExam => new(typeof(LoanExam).GetMethod("LoanExam")!);
    }
}
