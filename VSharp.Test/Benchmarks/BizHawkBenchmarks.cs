using NUnit.Framework;

namespace VSharp.Test.Benchmarks;

[TestFixture]
public class BizhawkBenchmarks
{
    private BenchmarkTarget _method = BizHawkTargets.NymaCore.DoInit;

    [Test]
    public void SmokeTest()
    {
        var stepsLimit = 30000u;
        Assert.True(
            Benchmarks.RunBenchmark(
                _method,
                VSharp.SearchStrategy.ExecutionTreeContributedCoverage,
                out var statistics,
                stepsLimit: stepsLimit,
                releaseBranches: false,
                randomSeed: 0,
                renderAndBuildTests: true
            )
        );
        var coverage = Benchmarks.GetMethodCoverage(_method, statistics);
        TestContext.Out.WriteLine($"Coverage: {coverage}%");
    }
}
