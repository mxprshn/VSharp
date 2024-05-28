using System.Reflection;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using Microsoft.Extensions.Logging;
using Mono.Cecil;
using Mono.Cecil.Rocks;

namespace VSharp.MethodSequencesBenchmarks;

public class SourceResolver
{
    private Dictionary<Type, List<string>> sourceFilesCache = new();
    private Dictionary<string, SyntaxNode> syntaxTreeCache = new();
    private Dictionary<string, AssemblyDefinition> assemblyCache = new();


    private AssemblyDefinition GetAssemblyDefinition(string location)
    {
        AssemblyDefinition definition;
        /*if (assemblyCache.TryGetValue(location, out definition))
        {
            return definition;
        }*/

        var p = new ReaderParameters
        {
            ReadSymbols = true
        };

        definition = AssemblyDefinition.ReadAssembly(location, p);
        //assemblyCache[location] = definition;
        return definition;
    }

    private SyntaxNode ReadSyntaxTreeFromFile(string sourceFilePath)
    {
        SyntaxNode syntaxTree;

        if (syntaxTreeCache.TryGetValue(sourceFilePath, out syntaxTree))
        {
            return syntaxTree;
        }

        using var stream = File.OpenRead(sourceFilePath);
        var parseOptions = new CSharpParseOptions(documentationMode: DocumentationMode.None);
        syntaxTree = CSharpSyntaxTree.ParseText(SourceText.From(stream), parseOptions).GetRoot();
        syntaxTreeCache[sourceFilePath] = syntaxTree;
        return syntaxTree;
    }

    private List<string> ResolveSourceFiles(Type type)
    {
        List<string> sourceFiles;

        if (sourceFilesCache.TryGetValue(type, out sourceFiles))
        {
            return sourceFiles;
        }

        var assemblyDefinition = GetAssemblyDefinition(type.Assembly.Location);

        var typeDefinition = assemblyDefinition.MainModule.GetAllTypes()
            .Single(t => t.MetadataToken.ToInt32() == type.MetadataToken);

        sourceFiles =
            typeDefinition.Methods
                .SelectMany(m => m.DebugInformation.GetSequencePointMapping().Values)
                .Select(sp => sp.Document.Url)
                .Distinct()
                .ToList();

        sourceFilesCache[type] = sourceFiles;
        return sourceFiles;
    }

    public List<SyntaxNode> GetSyntaxTrees(Type type)
    {
        var sourceFiles = ResolveSourceFiles(type);

        if (sourceFiles.Count == 0)
        {
            throw new Exception($"Cannot resolve source files for {type}");
        }

        return sourceFiles.Select(ReadSyntaxTreeFromFile).ToList();
    }

    private BaseMethodDeclarationSyntax? GetBaseDeclaration(MethodBase methodBase)
    {
        using var assemblyDefinition = GetAssemblyDefinition(methodBase.Module.Assembly.Location);

        var methodDefinition = assemblyDefinition.MainModule.GetAllTypes()
            .SingleOrDefault(t => t.MetadataToken.ToInt32() == methodBase.ReflectedType.MetadataToken)
            ?.Methods // Are constructors included?
            .SingleOrDefault(m => m.MetadataToken.ToInt32() == methodBase.MetadataToken);

        if (methodDefinition is null)
        {
            //logger.LogWarning($"Cannot resolve definition of method {methodBase}");
            return null;
        }

        if (!methodDefinition.DebugInformation.HasSequencePoints)
        {
            //logger.LogWarning($"Method definition for {methodBase} has no sequence points");
            return null;
        }

        BaseMethodDeclarationSyntax? decl = null;

        foreach (var seqPoint in methodDefinition.DebugInformation.GetSequencePointMapping().Values)
        {
            var firstLineNumber = seqPoint.StartLine;

            var sourceFile = seqPoint.Document.Url;
            var syntaxTree = ReadSyntaxTreeFromFile(sourceFile);

            var text = syntaxTree.GetText();
            var lines = text.Lines.Where(l => l.LineNumber == firstLineNumber - 1).ToList();

            if (lines.Count == 0)
            {
                continue;
            }

            decl = syntaxTree.DescendantNodes(lines[0].Span)
                .OfType<BaseMethodDeclarationSyntax>().FirstOrDefault();

            if (decl is not null)
            {
                break;
            }
        }

        if (decl is null)
        {
            // Default constructor case
            //logger.LogWarning($"Cannot find method declaration for {methodBase}");
            return null;
        }

        return decl;
    }

    public MethodDeclarationSyntax? GetSyntaxTree(MethodInfo methodInfo)
    {
        var baseDecl = GetBaseDeclaration(methodInfo);
        return baseDecl as MethodDeclarationSyntax;
    }

    public ConstructorDeclarationSyntax? GetSyntaxTree(ConstructorInfo methodInfo)
    {
        var baseDecl = GetBaseDeclaration(methodInfo);
        return baseDecl as ConstructorDeclarationSyntax;
    }

    public void Dispose()
    {
        foreach (var assembly in assemblyCache.Values)
        {
            assembly.Dispose();
        }

        assemblyCache.Clear();
    }
}
