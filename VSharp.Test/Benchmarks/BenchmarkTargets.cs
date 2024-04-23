using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using NUnit.Framework;
using VSharp.CSharpUtils;

namespace VSharp.Test.Benchmarks;

public abstract class BenchmarkTargets
{

    private readonly TextWriter _logWriter;
    private readonly string _benchmarksPath;

    public string Suite { get; private set; }
    public string DllFileName { get; private set; }

    public BenchmarkTargets(TextWriter logWriter, string benchmarksPath, string suite, string dllFileName)
    {
        _logWriter = logWriter;
        _benchmarksPath = benchmarksPath;
        Suite = suite;
        DllFileName = dllFileName;
    }

    public Assembly GetAssembly()
    {
        var assemblyPath = Path.Combine(_benchmarksPath, Suite, $"{DllFileName}.dll");
        return AssemblyManager.LoadFromAssemblyPath(assemblyPath);
    }

    public BenchmarkTarget TargetForMethod(MethodBase methodBase)
    {
        return new BenchmarkTarget(methodBase, Suite);
    }

    public IEnumerable<BenchmarkTarget> TargetsForMethods(string typeName, params string[] methodNames)
    {
        var type = GetAssembly().GetTypesChecked().FirstOrDefault(t => t.Name == typeName);
        if (type == null)
        {
            yield break;
        }

        foreach (var methodName in methodNames)
        {
            var methods = type.EnumerateExplorableMethods()
                .Where(m => methodName == m.Name || methodName == m.MetadataToken.ToString()).ToList();

            if (methods.Count > 1)
            {
                _logWriter.WriteLine($"Found more than one method {methodName}");
            }

            if (methods.Count == 0)
            {
                _logWriter.WriteLine($"No {methodName} methods found");
            }

            foreach (var method in methods)
            {
                yield return TargetForMethod(method);
            }
        }
    }

    public abstract IEnumerable<BenchmarkTarget> All();
}
