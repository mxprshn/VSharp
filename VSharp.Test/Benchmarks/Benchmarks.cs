#nullable enable
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Xml.Serialization;
using ConsoleTables;
using NUnit.Framework;
using VSharp.Core;
using VSharp.CoverageTool;
using VSharp.CSharpUtils;
using VSharp.Explorer;
using VSharp.Interpreter.IL;
using VSharp.TestRenderer;

namespace VSharp.Test.Benchmarks;

public static class Benchmarks
{
    private static readonly string TestRunnerPath = typeof(TestRunner.TestRunner).Assembly.Location;

    private static readonly DirectoryInfo RenderedTestsDirectory =
        new(Path.Combine(Directory.GetCurrentDirectory(), "RenderedTests"));

    private static bool TryBuildGeneratedTests()
    {
        var testsDir = RenderedTestsDirectory.GetDirectories("*.Tests").Single();
        var info = new ProcessStartInfo
        {
            WorkingDirectory = testsDir.FullName,
            FileName = DotnetExecutablePath.ExecutablePath,
            Arguments = "build"
        };
        var process = new Process();
        process.StartInfo = info;
        process.Start();
        process.WaitForExit();
        return process.IsSuccess();
    }

    private static BenchmarkResult RunInternal(
        TextWriter logWriter,
        BenchmarkTarget target,
        ExplorationOptions options,
        UnitTests unitTests,
        bool renderAndBuildTests,
        bool calculateCoverage)
    {
        if (target.Method is null)
        {
            throw new NotImplementedException("Running non single method benchmarks is not implemented yet");
        }

        if (RenderedTestsDirectory.Exists)
        {
            Directory.Delete(RenderedTestsDirectory.FullName, true);
        }

        Directory.CreateDirectory(RenderedTestsDirectory.FullName);

        var exploredMethodInfo = AssemblyManager.NormalizeMethod(target.Method);

        Logger.configureWriter(logWriter);

        var reporter = new TestReporter(unitTests, logWriter);

        using var explorer = new Explorer.Explorer(options, reporter);

        explorer.StartExploration(
            new[] { exploredMethodInfo },
            global::System.Array.Empty<Tuple<MethodBase, EntryPointConfiguration>>()
        );

        var failedTestNames = new List<string>();
        var result = new BenchmarkResult(
            false,
            explorer.Statistics,
            unitTests,
            target,
            reporter.States,
            true,
            true
        );

        explorer.Statistics.PrintDebugStatistics(logWriter);
        logWriter.WriteLine($"Test results written to {unitTests.TestDirectory.FullName}");

        logWriter.WriteLine($"Generated tests count: {unitTests.UnitTestsCount}");
        logWriter.WriteLine($"Found errors count: {unitTests.ErrorsCount}");

        if (unitTests is { UnitTestsCount: 0, ErrorsCount: 0 })
        {
            return result with { IsSuccessful = true };
        }

        var testsDir = unitTests.TestDirectory;
        if (renderAndBuildTests)
        {
            var tests = testsDir.EnumerateFiles("*.vst");
            logWriter.WriteLine("Starting tests renderer...");
            try
            {
                Renderer.Render(tests, true, false, exploredMethodInfo.DeclaringType,
                    outputDir: RenderedTestsDirectory);
            }
            catch (UnexpectedExternCallException)
            {
                // TODO: support rendering for extern mocks
            }
            catch (Exception e)
            {
                logWriter.WriteLine($"[RENDER ERROR]: {e}");
                return result with { IsRenderingSuccessful = false };
            }

            if (!TryBuildGeneratedTests())
            {
                logWriter.WriteLine($"[BUILD]: Cannot build generated tests");
                return result with { IsRenderingSuccessful = false };
            }

            result = result with { IsRenderingSuccessful = true };
        }

        if (!TestRunner.TestRunner.ReproduceTests(unitTests.TestDirectory))
        {
            return result with { AllTestsPassed = false };
        }

        result = result with { AllTestsPassed = true };

        if (calculateCoverage)
        {
            return result with { IsSuccessful = true, Coverage = GetMethodCoverage(result) };
        }

        return result with { IsSuccessful = true };
    }

    // TODO: Add support for fuzzing
    public static BenchmarkResult Run(
        TextWriter logWriter,
        BenchmarkTarget target,
        searchMode searchStrategy,
        string outputDir = "",
        bool createSubdir = true,
        int timeoutS = -1,
        uint stepsLimit = 0,
        bool releaseBranches = true,
        int randomSeed = -1,
        bool renderAndBuildTests = false,
        bool calculateCoverage = false)
    {
        var unitTests = new UnitTests(string.IsNullOrEmpty(outputDir) ? Directory.GetCurrentDirectory() : outputDir, createSubdir);

        var svmOptions = new SVMOptions(
            explorationMode: explorationMode.NewTestCoverageMode(coverageZone.MethodZone, searchStrategy),
            recThreshold: 1,
            solverTimeout: -1,
            visualize: false,
            releaseBranches: releaseBranches,
            maxBufferSize: 128,
            prettyChars: true,
            checkAttributes: false,
            stopOnCoverageAchieved: -1,
            randomSeed: randomSeed,
            stepsLimit: stepsLimit,
            savePathReplays: true
        );

        var explorationModeOptions = Explorer.explorationModeOptions.NewSVM(svmOptions);

        var explorationOptions = new ExplorationOptions(
            timeout: timeoutS == -1 ? TimeSpanBuilders.Infinite : TimeSpanBuilders.FromSeconds(timeoutS),
            outputDirectory: unitTests.TestDirectory,
            explorationModeOptions: explorationModeOptions
        );

        return RunInternal(logWriter, target, explorationOptions, unitTests, renderAndBuildTests, calculateCoverage);
    }

    public static BenchmarkResult Replay(TextWriter logWriter, BenchmarkTarget target, string replayPath, string outputDir)
    {
        var unitTests = new UnitTests(outputDir, createSubdir: false);
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

        return RunInternal(logWriter, target, explorationOptions, unitTests, false, false);
    }

    public static void PrintStatisticsComparison(List<(string Title, BenchmarkResult Results)> titleToResults)
    {
        var infos = titleToResults.SelectMany(tr =>
                tr.Results.Statistics.GeneratedTestInfos
                    .Where(ti => !ti.isError)
                    .Select(ti => (tr.Title, ti)))
            .OrderBy(tti => tti.ti.coverage);

        var header = new List<string> { "" };
        header.AddRange(titleToResults.Select(s => s.Title));
        var totalStatsTable = new ConsoleTable(header.ToArray());

        var timeRow = new List<string> { "Elapsed time" };
        timeRow.AddRange(titleToResults.Select(tr => tr.Results.Statistics.CurrentExplorationTime.ToString()));
        totalStatsTable.AddRow(timeRow.ToArray());

        var stepsRow = new List<string> { "Steps count" };
        stepsRow.AddRange(titleToResults.Select(tr => tr.Results.Statistics.StepsCount.ToString()));
        totalStatsTable.AddRow(stepsRow.ToArray());

        var testsCountRow = new List<string> { "Tests generated" };
        testsCountRow.AddRange(titleToResults.Select(tr => tr.Results.Tests.UnitTestsCount.ToString()));
        totalStatsTable.AddRow(testsCountRow.ToArray());

        var errorsCountRow = new List<string> { "Errors found" };
        errorsCountRow.AddRange(titleToResults.Select(tr => tr.Results.Tests.ErrorsCount.ToString()));
        totalStatsTable.AddRow(errorsCountRow.ToArray());

        var coverageRow = new List<string> { "Total coverage (with tool)" };
        coverageRow.AddRange(titleToResults.Select(tr => $"{tr.Results.Coverage}%"));
        totalStatsTable.AddRow(coverageRow.ToArray());

        totalStatsTable.Write();

        var testsStatsTableHeader = new List<string> { "Steps count" };
        testsStatsTableHeader.AddRange(titleToResults.Select(tr => tr.Title));
        var testsStatsTable = new ConsoleTable(testsStatsTableHeader.ToArray());

        foreach (var (title, info) in infos)
        {
            var row = new List<string> { info.stepsCount.ToString() };
            foreach (var (columnHeader, _) in titleToResults)
            {
                row.Add(title == columnHeader ? info.coverage.ToString("0.##") : "");
            }

            testsStatsTable.AddRow(row.ToArray());
        }

        testsStatsTable.Write();
    }

    private static int GetMethodCoverage(BenchmarkResult result)
    {
        if (result.Target.Method is null)
        {
            throw new Exception("Cannot get coverage of BenchmarkTarget without single method");
        }

        var runnerWithArgs = $"{TestRunnerPath} {result.Tests.TestDirectory}";
        var coverageTool = new PassiveCoverageTool(result.Tests.TestDirectory, result.Target.Method);
        return coverageTool.RunWithCoverage(runnerWithArgs);
    }

    public static Assembly LoadBenchmarkAssembly(string suite, string dllFileName)
    {
        var dllPath = TestContext.Parameters["BenchmarkDllsPath"];
        if (dllPath is null)
        {
            throw new Exception("Cannot read dll directory path from test context parameters");
        }

        var assemblyPath = Path.Combine(dllPath, suite, $"{dllFileName}.dll");
        return AssemblyManager.LoadFromAssemblyPath(assemblyPath);
    }
}
