using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Xml.Serialization;
using Grpc.Net.Client.Configuration;
using IntegrationTests;
using Microsoft.FSharp.Collections;
using NUnit.Framework;
using VSharp.CSharpUtils;
using VSharp.Explorer;

namespace VSharp.Test;

[TestFixture]
public class PathReplayTests
{
    [TestCaseSource(nameof(TestCases))]
    public void CheckReplayedPath(string typeName, string methodName, string replayPath, Action<UnitTest> checker)
    {
        var unitTests = new UnitTests(Directory.GetCurrentDirectory());
        var serializer = new XmlSerializer(typeof(pathReplay));
        var replay = serializer.Deserialize(File.OpenRead(replayPath)) as pathReplay;
        
        var svmOptions = new SVMOptions(
            explorationMode: explorationMode.NewPathReplayMode(replay.forkIndices),
            recThreshold: 1,
            solverTimeout: -1,
            visualize: false,
            releaseBranches: false,
            maxBufferSize: 128,
            prettyChars: true,
            checkAttributes: false,
            stopOnCoverageAchieved: -1,
            randomSeed: 0,
            stepsLimit: 10000,
            savePathReplays: false
        );
        
        var explorationModeOptions = Explorer.explorationModeOptions.NewSVM(svmOptions);

        var explorationOptions = new ExplorationOptions(
            timeout: TimeSpanBuilders.Infinite,
            outputDirectory: unitTests.TestDirectory,
            explorationModeOptions: explorationModeOptions
        );

        var reporter = new TestReporter(unitTests);

        using var explorer = new Explorer.Explorer(explorationOptions, reporter);
        
        var assembly = Assembly.GetExecutingAssembly();
        var type = assembly.ResolveType(typeName);
        var method = type.GetMethod(methodName);

        explorer.StartExploration(
            new[] { method },
            global::System.Array.Empty<Tuple<MethodBase, EntryPointConfiguration>>()
        );

        var generatedTestFile = unitTests.TestDirectory.EnumerateFiles("*.vst").Single();
        var generatedTest = UnitTest.Deserialize(generatedTestFile.FullName);
        checker(generatedTest);
    }
    
    private static void ReturnValueEquals<T>(UnitTest test, T expected)
    {
        var returnValue = (T)test.Expected;
        Assert.AreEqual(expected, returnValue);
    }
    
    public static IEnumerable<object[]> TestCases()
    {
        var rootTestsDirectory = new DirectoryInfo(TestContext.CurrentContext.TestDirectory);
        rootTestsDirectory = rootTestsDirectory.Parent.Parent.Parent;
        var rootTestDataDirectory = Path.Combine(rootTestsDirectory.FullName, "Tests", "PathReplayTestsData");
        
        const string conditionalClassName = nameof(Conditional);
        const string declareAfterReturnName = nameof(Conditional.DeclareAfterReturn);
        var declareAfterReturnDir = Path.Combine(rootTestDataDirectory, "DeclareAfterReturn");
        var loanExamDir = Path.Combine(rootTestDataDirectory, "LoanExam");
        yield return new object[]
        {
            conditionalClassName,
            declareAfterReturnName,
            Path.Combine(declareAfterReturnDir, "expected42.vsr"),
            (UnitTest ut) => { ReturnValueEquals(ut, 42); }
        };
        yield return new object[]
        {
            conditionalClassName,
            declareAfterReturnName,
            Path.Combine(declareAfterReturnDir, "falseFalse.vsr"),
            (UnitTest ut) => { ReturnValueEquals(ut, 0); }
        };
        yield return new object[]
        {
            conditionalClassName,
            declareAfterReturnName,
            Path.Combine(declareAfterReturnDir, "falseTrue.vsr"),
            (UnitTest ut) => { ReturnValueEquals(ut, 0); }
        };
    }
}