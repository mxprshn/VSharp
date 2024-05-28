using System.CommandLine;
using System.Diagnostics;
using System.Reflection;
using System.Text.Json;
using Microsoft.AspNetCore.Mvc.ApplicationModels;
using NUnit.Framework;
using ShellProgressBar;
using VSharp.Explorer;
using VSharp.MethodSequences;
using VSharp.Test;
using VSharp.Test.Benchmarks;
using VSharp.TestRunner;

namespace VSharp.MethodSequencesBenchmarks;

class Program
{
    private const int TestGenerationTimeoutS = 120;
    private const int TotalMethodSequenceSearchTimeoutS = 360;
    private const int RandomSeed = 42;

    // TODO: Replay sometimes sucks
    private static IReadOnlyList<TestInfo> TrueReplay(BenchmarkTarget target, string outputPath, string replayDirName)
    {
        var targetPath = Path.Combine(outputPath, target.SuiteName, Utils.GetTargetDirName(target));
        var testInfos = new List<TestInfo>();

        var replayDirPath = Path.Combine(targetPath, replayDirName);
        Directory.CreateDirectory(replayDirPath);
        var logsFilePath = Path.Combine(replayDirPath, "logs.log");
        using var logFileStream = File.Create(logsFilePath);
        using var logFileWriter = new StreamWriter(logFileStream);
        Console.WriteLine($"Found replays for method {target.Method}, replaying them...");

        var replays = Directory.EnumerateFiles(targetPath, "*.vsr").ToList();

        using (var pb = new ProgressBar(replays.Count, "Replaying...", ConsoleColor.Green))
        {
            foreach (var replayPath in replays)
            {
                pb.Message = $"Replaying {replayPath}...";
                var result = Benchmarks.Replay(logFileWriter, target, replayPath, replayDirPath);
                var name = Path.GetFileNameWithoutExtension(replayPath);
                var testInfo = result.TestInfos.Single() with { Name = name };
                testInfos.Add(testInfo);
                pb.Tick();
            }
        }

        return testInfos;
    }

    private static IReadOnlyList<TestInfo> RunTestsGeneration(BenchmarkTarget target, string outputPath, string replayDirName)
    {
        var targetPath = Path.Combine(outputPath, target.SuiteName, Utils.GetTargetDirName(target));
        var testInfos = new List<TestInfo>();
        var executionTreeContributedCoverageMode = searchMode.NewInterleavedMode(searchMode.ExecutionTreeMode, 1, searchMode.ContributedCoverageMode, 1);

        if (Directory.Exists(targetPath))
        {
            targetPath = Path.Combine(targetPath, replayDirName);
        }

        Directory.CreateDirectory(targetPath);
        var logsFilePath = Path.Combine(targetPath, "logs.log");
        using var logFileStream = File.Create(logsFilePath);
        using var logFileWriter = new StreamWriter(logFileStream);

        BenchmarkResult result;

        using (var pb = new FixedDurationBar(TimeSpan.FromSeconds(TestGenerationTimeoutS), $"Generating tests for {target.Method}"))
        {
            result = Benchmarks.Run(logFileWriter, target, executionTreeContributedCoverageMode, targetPath, createSubdir: false, timeoutS: TestGenerationTimeoutS, renderAndBuildTests: true, calculateCoverage: true, randomSeed: RandomSeed);
            // TODO: save some stats here
        }

        testInfos.AddRange(result.TestInfos);

        var stats = new MethodStatistics
        {
            GenerationTimeMillis = (uint)result.Statistics.CurrentExplorationTime.TotalMilliseconds,
            CoveragePercent = result.Coverage ?? -1,
            StepsCount = result.Statistics.StepsCount,
            InternalFails = result.Statistics.InternalFails.Select(e => e.ToString()).ToList(),
            AllTestsPassed = result.AllTestsPassed,
            IsRenderingSuccessful = result.IsRenderingSuccessful
        };
        var statsFilePath = Path.Combine(targetPath, "stats.json");
        using var statsFileStream = File.Create(statsFilePath);
        using var statsFileWriter = new StreamWriter(statsFileStream);
        statsFileWriter.Write(JsonSerializer.Serialize(stats));

        return testInfos;
    }

    private static bool RunMethodSequenceGeneration(TestInfo testInfo, BenchmarkTarget target, string outputPath, string replayDirName, TimeSpan timeout)
    {
        var stats = new SequenceStatistics();
        var result = true;
        var targetPath = Path.Combine(outputPath, target.SuiteName, Utils.GetTargetDirName(target));
        var replayDirPath = Path.Combine(targetPath, replayDirName);
        Directory.CreateDirectory(replayDirPath);

        if (!SupportValidation.IsSupported(testInfo.State))
        {
            stats.IsUnsupported = true;
        }
        else
        {
            try
            {
                // TODO: we may have problems with static state
                var searcher = new MethodSequenceSearcher(testInfo.State);
                var stopwatch = new Stopwatch();
                stopwatch.Start();
                var stepsCount = 0u;
                while (stopwatch.Elapsed < timeout)
                {
                    var foundSequences = searcher.MakeStep();
                    stepsCount++;
                    if (foundSequences == null)
                    {
                        break;
                    }

                    if (foundSequences.Count <= 0) continue;

                    stopwatch.Stop();
                    var sequence = foundSequences.First();
                    stats.SequenceLength = (uint)sequence.elements.Length;

                    var sequenceFilePath = Path.Combine(replayDirPath, $"{testInfo.Name}.seq");
                    using var sequenceFileStream = File.Create(sequenceFilePath);
                    using var sequenceFileWriter = new StreamWriter(sequenceFileStream);
                    sequenceFileWriter.Write(sequence.ToString());
                    break;
                }

                var internalFails = new Dictionary<string, List<string>>();
                foreach (var (method, methodFails) in searcher.DumpInternalFails())
                {
                    internalFails[method.ToString()] = methodFails.Select(f => f.ToString()).ToList();
                }

                stats.ExplorationExceptions = internalFails;
                stats.CompositionExceptions = searcher.DumpSearcherFails().Select(f => f.ToString()).ToList();

                stats.StepsCount = stepsCount;
                stats.GenerationTimeMs = (uint)stopwatch.Elapsed.TotalMilliseconds;

                if (stats.SequenceLength <= 0)
                {
                    StatisticsCollector.DumpSequences(replayDirPath, target, testInfo.Name, searcher.DumpCurrentSequences(), searcher.DumpDiscardedSequences());
                }
            }
            catch (InsufficientInformationException e)
            {
                Console.WriteLine(e.ToString());
                stats.IsUnsupported = true;
            }
            catch (Exception e)
            {
                Console.WriteLine(e.ToString());
                stats.CriticalException = e.ToString();
                result = false;
            }
        }

        var statsFilePath = Path.Combine(replayDirPath, $"{testInfo.Name}.json");
        using var statsFileStream = File.Create(statsFilePath);
        using var statsFileWriter = new StreamWriter(statsFileStream);
        statsFileWriter.Write(JsonSerializer.Serialize(stats));
        return result;
    }

    static void Main(string[] args)
    {
        var runCommand =
            new Command("run", "Runs benchmark");
        var benchmarkIdArgument =
            new Argument<string>("id", description: "Benchmark id to run");
        var benchmarksPathArgument =
            new Argument<DirectoryInfo>("benchmarks-path", description: "Path to the root directory with benchmarks dll-s");
        var outputOption = new Option<DirectoryInfo>(
            aliases: new[] { "--output", "-o" },
            () => new DirectoryInfo(Directory.GetCurrentDirectory()),
            "Path where benchmark artifacts are saved");
        var replayToRunOption = new Option<string>(
            aliases: new[] { "--replay", "-r" },
            () => "",
            "Concrete replay file name to run benchmark on");
        var generateSequencesOption = new Option<bool>(
            aliases: new[] { "--gen-seqs", "-gs" },
            () => true,
            "Generate method sequences or not");

        runCommand.AddArgument(benchmarkIdArgument);
        runCommand.AddArgument(benchmarksPathArgument);
        runCommand.AddOption(outputOption);
        runCommand.AddOption(generateSequencesOption);
        runCommand.AddOption(replayToRunOption);

        var benchmarkDllPathArgument =
            new Argument<FileInfo>("dll-path", description: "Path to the benchmark dll");
        var benchmarkKeyArgument =
            new Argument<string>("key", description: "Key name of the benchmark suite");

        var outputFileArgument = new Argument<FileInfo>("output");
        var shuffleSeedArgument = new Argument<int>("seed", description: "Seed to shuffle benchmarks");
        var takeNArgument = new Argument<int>("seed", description: "Seed to shuffle benchmarks");

        var detectBenchmarks =
            new Command("detect", "Finds all methods suitable as benchmarks in assembly");
        detectBenchmarks.AddArgument(benchmarkDllPathArgument);
        detectBenchmarks.AddArgument(benchmarkKeyArgument);
        detectBenchmarks.AddArgument(shuffleSeedArgument);
        detectBenchmarks.AddArgument(takeNArgument);
        detectBenchmarks.AddArgument(outputFileArgument);

        var dllRunCommand =
            new Command("dll-run", "Runs benchmark resolving them from assembly");
        dllRunCommand.AddArgument(benchmarkDllPathArgument);
        dllRunCommand.AddArgument(benchmarkIdArgument);
        dllRunCommand.AddArgument(benchmarkKeyArgument);
        dllRunCommand.AddOption(outputOption);

        var listCommand =
            new Command("list", "Lists available benchmarks");
        listCommand.AddArgument(benchmarksPathArgument);

        var statsCollectionCommand =
            new Command("stats", "Collects statistics and plots graphs");
        var benchmarkIdsFileArgument = new Argument<FileInfo>("ids", description: "File with benchmark ids to collect stats for");
        var runsPathDirArgument = new Argument<DirectoryInfo>("runs", description: "Directory with benchmarks runs");
        statsCollectionCommand.AddArgument(benchmarkIdsFileArgument);
        statsCollectionCommand.AddArgument(runsPathDirArgument);
        statsCollectionCommand.AddArgument(benchmarksPathArgument);
        statsCollectionCommand.AddOption(outputOption);

        detectBenchmarks.SetHandler(context =>
        {
            var parseResult = context.ParseResult;
            var benchmarksDllPath = parseResult.GetValueForArgument(benchmarkDllPathArgument);
            var outputPath = parseResult.GetValueForArgument(outputFileArgument);
            var seed = parseResult.GetValueForArgument(shuffleSeedArgument);
            var takeN = parseResult.GetValueForArgument(takeNArgument);
            var key = parseResult.GetValueForArgument(benchmarkKeyArgument);

            var benchmarkSelector = new BenchmarkSelector();
            var assembly = AssemblyManager.LoadFromAssemblyPath(benchmarksDllPath.FullName);
            using var outputFile = File.Create(outputPath.FullName);
            using var fileWriter = new StreamWriter(outputFile);

            var random = new Random(seed);
            var benches = benchmarkSelector.GetBenchmarkTargetsFromAssembly(assembly, key)
                .ToList()
                .OrderBy(_ => random.Next())
                .Take(takeN)
                .ToList();

            foreach (var bench in benches)
            {
                fileWriter.WriteLine(bench.Id);
            }
        });

        dllRunCommand.SetHandler(context =>
        {
            var parseResult = context.ParseResult;
            var dllPath = parseResult.GetValueForArgument(benchmarkDllPathArgument);
            var benchKey = parseResult.GetValueForArgument(benchmarkKeyArgument);
            var benchId = parseResult.GetValueForArgument(benchmarkIdArgument);
            var outputPath = parseResult.GetValueForOption(outputOption);

            var resolver = new BenchmarkSelector();
            var targets =
                resolver.GetBenchmarkTargetsFromAssembly(AssemblyManager.LoadFromAssemblyPath(dllPath.FullName), benchKey);
            var target = targets.Single(t => t.Id == benchId);

            var replayDirName = DateTime.Now.ToString("yyyy-MM-dd_HH.mm.ss");
            var tests = RunTestsGeneration(target, outputPath.FullName, replayDirName);

            using (var pb = new ProgressBar(tests.Count, "Generating sequence...", ConsoleColor.Green))
            {
                var timeoutPerTest = int.Min(TotalMethodSequenceSearchTimeoutS * 1000 / tests.Count, 15000);
                foreach (var test in tests)
                {
                    pb.Message = $"Generating sequence for {test.Name}...";
                    var success = RunMethodSequenceGeneration(test, target, outputPath.FullName, replayDirName, TimeSpan.FromMilliseconds(timeoutPerTest));
                    if (!success)
                    {
                        pb.ForegroundColor = ConsoleColor.Yellow;
                    }

                    pb.Tick();
                }
            }
        });

        runCommand.SetHandler(context =>
        {
            var parseResult = context.ParseResult;
            var outputPath = parseResult.GetValueForOption(outputOption);
            var benchmarksPath = parseResult.GetValueForArgument(benchmarksPathArgument);
            var benchmarkId = parseResult.GetValueForArgument(benchmarkIdArgument);
            var generateSequences = parseResult.GetValueForOption(generateSequencesOption);
            var concreteReplayName = parseResult.GetValueForOption(replayToRunOption);

            var targets = new BenchmarkTargets(benchmarksPath.FullName);
            var target = targets.GetById(benchmarkId);

            var replayDirName = DateTime.Now.ToString("yyyy-MM-dd_HH.mm.ss");
            var tests = RunTestsGeneration(target, outputPath.FullName, replayDirName);

            if (!string.IsNullOrEmpty(concreteReplayName))
            {
                tests = tests.Where(t => t.Name == concreteReplayName).ToList();
            }

            if (!generateSequences)
            {
                return;
            }

            using (var pb = new ProgressBar(tests.Count, "Generating sequence...", ConsoleColor.Green))
            {
                var timeoutPerTest = int.Min(TotalMethodSequenceSearchTimeoutS * 1000 / tests.Count, 15000);
                foreach (var test in tests)
                {
                    pb.Message = $"Generating sequence for {test.Name}...";
                    var success = RunMethodSequenceGeneration(test, target, outputPath.FullName, replayDirName, TimeSpan.FromMilliseconds(timeoutPerTest));
                    if (!success)
                    {
                        pb.ForegroundColor = ConsoleColor.Yellow;
                    }

                    pb.Tick();
                }
            }

        });

        listCommand.SetHandler(context =>
        {
            var parseResult = context.ParseResult;
            var benchmarksPath = parseResult.GetValueForArgument(benchmarksPathArgument);
            var targets = new BenchmarkTargets(benchmarksPath.FullName);
            targets.PrintAvailableBenchmarks(false);
        });

        statsCollectionCommand.SetHandler(context =>
        {
            var parseResult = context.ParseResult;
            var idsFile = parseResult.GetValueForArgument(benchmarkIdsFileArgument);
            var benchmarksPath = parseResult.GetValueForArgument(benchmarksPathArgument);
            var runsPath = parseResult.GetValueForArgument(runsPathDirArgument);
            var output = parseResult.GetValueForOption(outputOption);
            var targets = new BenchmarkTargets(benchmarksPath.FullName);
            var collector = new StatisticsCollector(targets, idsFile, runsPath, output);
            collector.SaveSequenceFailuresReport();
            collector.SequenceCountHistogram();
        });

        var rootCommand = new RootCommand();
        rootCommand.AddCommand(runCommand);
        rootCommand.AddCommand(listCommand);
        rootCommand.AddCommand(statsCollectionCommand);
        rootCommand.AddCommand(detectBenchmarks);
        rootCommand.AddCommand(dllRunCommand);
        rootCommand.Invoke(args);
    }
}
