using ConsoleTables;
using VSharp.Test.Benchmarks;

namespace VSharp.MethodSequencesBenchmarks;

public class BenchmarkTargets
{
    private readonly Dictionary<string, BenchmarkTarget> _targetsById = new();

    public BenchmarkTargets(string benchmarksPath)
    {
        var btcPayServerTargets = new BtcPayServerTargets(Console.Out, benchmarksPath);
        var loanExamTarget = VSharpTargets.LoanExam();
        var openRaTargets = new OpenRaTargets(Console.Out, benchmarksPath);
        var osuTargets = new OsuTargets(Console.Out, benchmarksPath);
        foreach (var target in btcPayServerTargets.All().Concat(openRaTargets.All()).Concat(osuTargets.All()).Concat(loanExamTarget))
        {
            _targetsById[target.Id] = target;
        }
    }

    public BenchmarkTarget GetById(string id)
    {
        return _targetsById[id];
    }

    public void PrintAvailableBenchmarks(bool printNames)
    {
        if (!printNames)
        {
            foreach (var idAndTarget in _targetsById)
            {
                Console.WriteLine($"\"{idAndTarget.Key}\"");
            }
            return;
        }

        Console.WriteLine("AVAILABLE TARGETS:");
        Console.WriteLine();
        foreach (var idAndTarget in _targetsById)
        {
            Console.WriteLine($"Id: {idAndTarget.Key}");
            Console.WriteLine($"Target: {idAndTarget.Value.Method}");
            Console.WriteLine();
        }
    }
}
