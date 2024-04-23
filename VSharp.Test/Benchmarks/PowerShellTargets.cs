using System.Collections.Generic;
using System.Reflection;

namespace VSharp.Test.Benchmarks;

public class PowerShellTargets
{
    private const string PowerShellSuiteName = "powershell";
    private const string CommandsUtilityDllName = "Microsoft.PowerShell.Commands.Utility";

    // Problems with mock composition
    public static BenchmarkTarget AddMemberCommand()
    {
        var assembly = Benchmarks.LoadBenchmarkAssembly(PowerShellSuiteName, CommandsUtilityDllName);
        var cls = assembly.GetType("Microsoft.PowerShell.Commands.AddMemberCommand");
        var method = cls.GetMethod("ProcessRecord", BindingFlags.Instance | BindingFlags.NonPublic);
        return new BenchmarkTarget(method, PowerShellSuiteName);
    }

    // Problems with mock composition
    public static BenchmarkTarget EnablePSBreakpointCommand()
    {
        var assembly = Benchmarks.LoadBenchmarkAssembly(PowerShellSuiteName, CommandsUtilityDllName);
        var cls = assembly.GetType("Microsoft.PowerShell.Commands.EnablePSBreakpointCommand");
        var method = cls.GetMethod("ProcessBreakpoint", BindingFlags.Instance | BindingFlags.NonPublic);
        return new BenchmarkTarget(method, PowerShellSuiteName);
    }
}
