using System.Collections.Generic;
using System.IO;
using System.Linq;
using NotImplementedException = System.NotImplementedException;

namespace VSharp.Test.Benchmarks;

public class ScottPlotTargets : BenchmarkTargets
{
    public ScottPlotTargets(TextWriter logWriter, string benchmarksPath) :
        base(logWriter, benchmarksPath, "scottplot", "ScottPlot")
    {
    }

    public override IEnumerable<BenchmarkTarget> All()
    {
        return BenchmarksSearcher.GetForAssembly(GetAssembly()).Select(m => new BenchmarkTarget(m, Suite));
    }
}
