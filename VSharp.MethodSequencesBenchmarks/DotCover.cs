using System;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Text;
using System.Xml;

namespace VSharp.CSharpUtils;

public readonly record struct CoverageStats(
    int TotalStatements,
    int CoveragePercent
);

public class DotCover
{
    public static bool RunForVSharpTests(DirectoryInfo dirWithTests, FileInfo reportFile, TextWriter outputWriter = null)
    {
        var testRunnerAssemblyPath = typeof(TestRunner.TestRunner).Assembly.Location;
        var filters = new[] { "-:module=Microsoft.*", "-:module=FSharp.*", "-:class=VSharp.*", "-:module=VSharp.Utils" };
        var args =
            $"dotcover --dcFilters=\"{string.Join(";", filters)}\" {testRunnerAssemblyPath} {dirWithTests} --dcReportType=DetailedXML --dcOutput={reportFile.FullName}";

        var info = new ProcessStartInfo
        {
            WorkingDirectory = Directory.GetCurrentDirectory(),
            FileName = "dotnet",
            Arguments = args,
            UseShellExecute = false,
            RedirectStandardOutput = true,
            RedirectStandardError = true
        };

        using (var proc = Process.Start(info))
        {
            proc.OutputDataReceived += (sender, e) => outputWriter?.WriteLine(e.Data);
            proc.ErrorDataReceived += (sender, e) => outputWriter?.WriteLine(e.Data);
            proc.BeginOutputReadLine();
            proc.BeginErrorReadLine();
            proc.WaitForExit();
            return proc.IsSuccess();
        }
    }

    private static string TypeNameForDotCover(Type type)
    {
        if (type.IsGenericType)
        {
            var args = type.GetGenericArguments();
            var definitionName = type.GetGenericTypeDefinition().FullName.Replace("+", ".");
            var index = definitionName.IndexOf("`");
            var trimmedName = index >= 0 ? definitionName.Substring(0, index) : definitionName;
            return $"{trimmedName}<{string.Join(",", args.Select(TypeNameForDotCover))}>";
        }

        if (type.IsGenericParameter)
        {
            return type.Name;
        }

        if (type.IsByRef)
        {
            return TypeNameForDotCover(type.GetElementType());
        }

        var name = type.FullName ?? type.Name;
        return name.Replace("+", ".");
    }

    private static string[] SplitTypeName(string typeName)
    {
        var names = typeName.Split('+');
        names[0] = names[0].Split('.').Last();
        return names;
    }

    private static string[] DeclaringTypeNameForDotCover(Type type)
    {
        if (type.IsGenericType)
        {
            var args = type.GetGenericArguments();
            var names = SplitTypeName(type.GetGenericTypeDefinition().FullName);
            string HandleGenericsInName(string name)
            {
                var index = name.IndexOf("`");
                var trimmedName = index >= 0 ? name.Substring(0, index) : name;
                return $"{trimmedName}<{string.Join(",", args.Select(TypeNameForDotCover))}>";
            }
            names[^1] = HandleGenericsInName(names[^1]);
            return names;
        }

        return SplitTypeName(type.FullName);
    }

    // TODO: support ctors
    public static CoverageStats GetMethodCoverageStatsFromReport(FileInfo reportFile, MethodInfo method)
    {
        if (string.IsNullOrEmpty(method.DeclaringType?.Namespace))
            throw new NotImplementedException("Coverage report handling for types without namespace not implemented.");

        var assemblyName = method.Module.Assembly.GetName().Name;
        var namespaceName = method.DeclaringType.Namespace;
        var typeNames = DeclaringTypeNameForDotCover(method.DeclaringType);
        var typeSection = string.Join("", typeNames.Select(t => $"/Type[@Name='{t}']"));
        var parametersTypes = string.Join(",", method.GetParameters().Select(p => TypeNameForDotCover(p.ParameterType)));
        var returnType = TypeNameForDotCover(method.ReturnType);
        var methodName = $"{method.Name}({parametersTypes}):{returnType}";
        var xpathRoot = $"/Root/Assembly[@Name='{assemblyName}']/Namespace[@Name='{namespaceName}']{typeSection}/Method[@Name='{methodName}']";

        var doc = new XmlDocument();
        var docPath = reportFile.FullName;
        if (!File.Exists(docPath))
            throw new FileNotFoundException($"File not found: {docPath}");

        doc.Load(docPath);
        var methodNodes = doc.DocumentElement.SelectNodes(xpathRoot);
        if (methodNodes.Count == 0)
        {
            throw new InvalidOperationException($"Coverage results for {method} not found!");
        }

        if (methodNodes.Count > 1)
        {
            throw new InvalidOperationException($"Invalid query of coverage results for {method}!");
        }

        var methodNode = methodNodes[0];

        var coveragePercent = int.Parse(methodNode.Attributes["CoveragePercent"].Value);
        var totalStatements = int.Parse(methodNode.Attributes["TotalStatements"].Value);

        return new CoverageStats(CoveragePercent: coveragePercent, TotalStatements: totalStatements);
    }
}
