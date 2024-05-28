using System.Collections.Generic;
using System.Reflection;
using VSharp.CSharpUtils;

namespace VSharp.Test.Benchmarks;

public class VSharpTargets
{
    public static IEnumerable<BenchmarkTarget> TaylorSwift()
    {
        var assembly = Assembly.GetExecutingAssembly();
        var type = assembly.ResolveType("MethodSequences");
        yield return new BenchmarkTarget(type.GetMethod("TaylorSwiftJetCost"), "vsharp");
    }

    public static IEnumerable<BenchmarkTarget> Strings()
    {
        var assembly = Assembly.GetExecutingAssembly();
        var type = assembly.ResolveType("Strings");
        return new List<BenchmarkTarget>
        {
            new(type.GetMethod("FormatInt"), "vsharp"),
            new(type.GetMethod("FormatUInt32"), "vsharp"),
            new(type.GetMethod("StringFormat2"), "vsharp")
        };
    }

    public static IEnumerable<BenchmarkTarget> MethodSequences()
    {
        var assembly = Assembly.GetExecutingAssembly();
        var type = assembly.ResolveType("MethodSequences");
        yield return new BenchmarkTarget(type.GetMethod("TwoSetters"), "vsharp");
    }

    public static IEnumerable<BenchmarkTarget> LoanExam()
    {
        var assembly = Assembly.GetExecutingAssembly();
        var type = assembly.ResolveType("LoanExam");
        return new List<BenchmarkTarget>
        {
            new(type.GetMethod("Build"), "vsharp")
        };
    }
}
