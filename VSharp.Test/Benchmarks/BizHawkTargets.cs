using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using VSharp.CSharpUtils;

namespace VSharp.Test.Benchmarks;

internal static class BizHawkTargets
{
    private const string BizHawkSuiteName = "bizhawk";
    private const string CoresDllName = "BizHawk.Emulation.Cores";

    public static class NymaCore
    {
        public static  BenchmarkTarget DoInit
        {
            get
            {
                var assembly = Benchmarks.LoadBenchmarkAssembly(BizHawkSuiteName, CoresDllName);
                var method = assembly.GetModules().First().ResolveType(0x200026D).GetMethods(BindingFlags.Instance | BindingFlags.NonPublic).First(m => m.MetadataToken == 0x6000149);
                return new(method);
            }
        }
    }

    public static IEnumerable<BenchmarkTarget> LR35902()
    {
        var assembly = Benchmarks.LoadBenchmarkAssembly(BizHawkSuiteName, CoresDllName);
        var type = assembly.ResolveType("LR35902");
        var method = type.GetMethod("ExecuteOne");
        return new List<BenchmarkTarget> { new(method) };
    }
}
