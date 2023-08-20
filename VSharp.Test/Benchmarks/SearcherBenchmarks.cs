using System.Collections.Generic;
using System.Linq;
using NUnit.Framework;
using VSharp.Interpreter.IL;

namespace VSharp.Test.Benchmarks;

[TestFixture, Explicit]
public class SearcherBenchmarks
{
    [Test]
    [TestCaseSource(typeof(VSharpTargets), nameof(VSharpTargets.LoanExam))]
    public void SearchersEvaluation(BenchmarkTarget target)
    {
        var stepsLimit = 40000u;
        TestContext.Out.WriteLine("Running BFS...");
        Assert.True(Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.BFS, out var bfsResults, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 0));
        TestContext.Out.WriteLine("Running DFS...");
        Assert.True(Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.DFS, out var dfsResults, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 0));
        TestContext.Out.WriteLine("Running Contributed coverage...");
        Assert.True(Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.ContributedCoverage, out var contributedCoverageResults, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 0));
        TestContext.Out.WriteLine("Running Shortest distance...");
        Assert.True(Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.ShortestDistance, out var shortestDistanceResults, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 0));
        TestContext.Out.WriteLine("Running Shortest distance (random)...");
        Assert.True(Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.RandomShortestDistance, out var randomShortestDistanceResults, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 0));
        TestContext.Out.WriteLine("Running Shortest distance (random, 2)...");
        Assert.True(Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.RandomShortestDistance, out var randomShortestDistanceResults2, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 100));
        TestContext.Out.WriteLine("Running Execution tree interleaved...");
        Assert.True(Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.ExecutionTreeContributedCoverage, out var executionTreeResultsInterleaved, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 0));
        TestContext.Out.WriteLine("Running Execution tree interleaved (2)...");
        Assert.True(Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.ExecutionTreeContributedCoverage, out var executionTreeResultsInterleaved2, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 100));
        TestContext.Out.WriteLine("Running Execution tree...");
        Assert.True(Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.ExecutionTree, out var executionTreeResults, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 0));
        TestContext.Out.WriteLine("Running Execution tree (2)...");
        Assert.True(Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.ExecutionTree, out var executionTreeResults2, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 100));
        var stats = new List<(string, Statistics, int)>
        {
            ("BFS", bfsResults, Benchmarks.GetMethodCoverage(target, bfsResults.OutputDir)),
            ("DFS", dfsResults, Benchmarks.GetMethodCoverage(target, dfsResults.OutputDir)),
            ("Contributed coverage", contributedCoverageResults, Benchmarks.GetMethodCoverage(target, contributedCoverageResults.OutputDir)),
            ("Shortest distance", shortestDistanceResults, Benchmarks.GetMethodCoverage(target, shortestDistanceResults.OutputDir)),
            ("Random shortest distance (1)", randomShortestDistanceResults, Benchmarks.GetMethodCoverage(target, randomShortestDistanceResults.OutputDir)),
            ("Random shortest distance (2)", randomShortestDistanceResults2, Benchmarks.GetMethodCoverage(target, randomShortestDistanceResults2.OutputDir)),
            ("Execution tree int (1)", executionTreeResultsInterleaved, Benchmarks.GetMethodCoverage(target, executionTreeResultsInterleaved.OutputDir)),
            ("Execution tree int (2)", executionTreeResultsInterleaved2, Benchmarks.GetMethodCoverage(target, executionTreeResultsInterleaved2.OutputDir)),
            ("Execution tree (1)", executionTreeResults, Benchmarks.GetMethodCoverage(target, executionTreeResults.OutputDir)),
            ("Execution tree (2)", executionTreeResults2, Benchmarks.GetMethodCoverage(target, executionTreeResults2.OutputDir)),
        };
        Benchmarks.PrintStatisticsComparison(stats);
    }

    [Test]
    public void BizHawkLR35902IsCoveredWithExecutionTreeInterleavedSearcher([Values(0, 42, 73)] int randomSeed)
    {
        var target = Targets.BizHawk.LR35902ExecuteOne;
        var stepsLimit = 30000u;
        Assert.True(
            Benchmarks.RunBenchmark(
                target,
                VSharp.SearchStrategy.ExecutionTreeContributedCoverage,
                out var statistics,
                stepsLimit: stepsLimit,
                releaseBranches: false,
                randomSeed: randomSeed,
                renderAndBuildTests: true
            )
        );
        var coverage = Benchmarks.GetMethodCoverage(target, statistics.OutputDir);
        TestContext.Out.WriteLine($"Coverage: {coverage}%");
        Assert.That(coverage, Is.GreaterThan(90));
    }

    [Test]
    public void UnityCollections()
    {
        var targets = Targets.Unity.Collections.All.ToList();
        TestContext.Progress.WriteLine($"Found {targets.Count} method targets in Unity.Collections");
        var i = 1;
        foreach (var target in targets)
        {
            TestContext.Progress.WriteLine($"Running V# on method {target} ({i} of {targets.Count})");
            i++;
            var result = Benchmarks.Run(
                target,
                searchMode.NewInterleavedMode(searchMode.ExecutionTreeMode, 1, searchMode.ContributedCoverageMode, 1),
                out var statistics,
                stepsLimit: 30000,
                releaseBranches: false,
                randomSeed: 0,
                renderAndBuildTests: true
            );
        }
    }

    [Test]
    public void UnityHash64()
    {
        var target = Targets.Unity.Collections.Hash64Internal;
        Assert.True(
            Benchmarks.Run(
                target,
                searchMode.NewInterleavedMode(searchMode.ExecutionTreeMode, 1, searchMode.ContributedCoverageMode, 1),
                out var statistics,
                stepsLimit: 30000,
                releaseBranches: false,
                randomSeed: 0,
                renderAndBuildTests: true
            )
        );
    }

    [Test]
    public void UnityHlslTree()
    {
        var targets = Targets.Unity.HighDefinition.HlslTree.Skip(4).ToList();
        TestContext.Progress.WriteLine($"Found {targets.Count} method targets");
        var i = 1;
        foreach (var target in targets)
        {
            TestContext.Progress.WriteLine($"Running V# on method {target} ({i} of {targets.Count})");
            i++;
            var result = Benchmarks.Run(
                target,
                searchMode.NewInterleavedMode(searchMode.ExecutionTreeMode, 1, searchMode.ContributedCoverageMode, 1),
                out var statistics,
                stepsLimit: 30000,
                releaseBranches: false,
                randomSeed: 0,
                renderAndBuildTests: true
            );
            if (!result)
            {
                TestContext.Progress.WriteLine("FAIL");
            }
        }
    }
}
