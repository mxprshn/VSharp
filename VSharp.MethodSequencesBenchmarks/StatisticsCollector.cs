using System.Text;
using System.Text.Json;
using ScottPlot;
using VSharp.Test.Benchmarks;

namespace VSharp.MethodSequencesBenchmarks;

public class StatisticsCollector
{
    private readonly List<string> _benchmarkIds;
    private readonly DirectoryInfo _runsDir;
    private readonly DirectoryInfo _outputDir;
    private readonly BenchmarkTargets _targets;
    private readonly string _timestamp;

    public StatisticsCollector(BenchmarkTargets targets, FileInfo benchmarkIdsFile, DirectoryInfo runsDir, DirectoryInfo outputDir)
    {
        _runsDir = runsDir;
        _outputDir = outputDir;
        _targets = targets;
        _timestamp = DateTime.Now.ToString("yyyy-MM-dd_HH.mm.ss");
        using var idsFileStream = benchmarkIdsFile.OpenRead();
        using var idsFileReader = new StreamReader(idsFileStream);
        var text = idsFileReader.ReadToEnd();
        _benchmarkIds = text.Split(new[] { Environment.NewLine }, StringSplitOptions.RemoveEmptyEntries).ToList();
    }

    public void SequenceCountHistogram()
    {
        var lengthToCount = new Dictionary<uint, int>();
        var timeoutedCount = 0;
        var failedCount = 0;
        foreach (var target in _benchmarkIds.Select(_targets.GetById))
        {
            var targetRootDirPath = Path.Combine(_runsDir.FullName, target.SuiteName, Utils.GetTargetDirName(target));
            var lastRunDir = Directory.EnumerateDirectories(targetRootDirPath, "2024*").OrderDescending().FirstOrDefault();
            if (lastRunDir == null)
            {
                continue;
            }

            foreach (var jsonFile in Directory.EnumerateFiles(lastRunDir, "*.json"))
            {
                var json = File.ReadAllText(jsonFile);
                var stats = JsonSerializer.Deserialize<SequenceStatistics>(json);

                if (stats.SequenceLength == 0u)
                {
                    if (string.IsNullOrEmpty(stats.Exception))
                    {
                        timeoutedCount++;
                    }
                    else
                    {
                        failedCount++;
                    }
                }
                else
                {
                    if (lengthToCount.TryGetValue(stats.SequenceLength, out var currentCount))
                    {
                        lengthToCount[stats.SequenceLength] = currentCount + 1;
                    }
                    else
                    {
                        lengthToCount[stats.SequenceLength] = 1;
                    }
                }
            }
        }

        Console.WriteLine($"Failed sequences count: {failedCount}");
        Console.WriteLine($"Timeouted sequences count: {timeoutedCount}");

        var plot = new Plot();
        var ticks = new List<Tick>();
        plot.Add.Bar(position: 1, value: failedCount);
        ticks.Add(new Tick(1, "Failed"));
        plot.Add.Bar(position: 2, value: timeoutedCount);
        ticks.Add(new Tick(2, "Timeout"));
        var currentBar = 3;
        foreach (var seqLength in lengthToCount.Keys.Order())
        {
            plot.Add.Bar(position: currentBar, value: lengthToCount[seqLength]);
            ticks.Add(new Tick(currentBar, seqLength.ToString()));
            currentBar++;
        }

        plot.Axes.Bottom.TickGenerator = new ScottPlot.TickGenerators.NumericManual(ticks.ToArray());
        plot.Axes.Bottom.MajorTickStyle.Length = 0;
        plot.HideGrid();

        //plot.Axes.Margins(bottom: 0);

        plot.SavePng(Path.Combine(_outputDir.FullName, $"{_timestamp}_histogram.png"), 400, 300);
    }

    public void SaveSequenceFailuresReport()
    {
        var failuresToBenches = new Dictionary<string, List<string>>();

        foreach (var target in _benchmarkIds.Select(_targets.GetById))
        {
            var targetRootDirPath = Path.Combine(_runsDir.FullName, target.SuiteName, Utils.GetTargetDirName(target));
            var lastRunDir = Directory.EnumerateDirectories(targetRootDirPath, "2024*").OrderDescending().FirstOrDefault();
            if (lastRunDir == null)
            {
                continue;
            }

            foreach (var jsonFile in Directory.EnumerateFiles(lastRunDir, "*.json"))
            {
                var json = File.ReadAllText(jsonFile);
                var stats = JsonSerializer.Deserialize<SequenceStatistics>(json);

                if (string.IsNullOrEmpty(stats.Exception)) continue;

                var exceptionString = stats.Exception;
                var targetString = $"{target.Id} {Path.GetFileName(jsonFile)}";
                if (failuresToBenches.TryGetValue(exceptionString, out var benches))
                {
                    benches.Add(targetString);
                }
                else
                {
                    var newList = new List<string> { targetString };
                    failuresToBenches[exceptionString] = newList;
                }
            }
        }

        var statsFileBuilder = new StringBuilder();

        foreach (var kvp in failuresToBenches.OrderByDescending(kvp => kvp.Value.Count))
        {
            statsFileBuilder.AppendLine("```");
            statsFileBuilder.AppendLine(kvp.Key);
            statsFileBuilder.AppendLine("```");
            statsFileBuilder.AppendLine($"Sequences with this error count: {kvp.Value.Count}:");
            foreach (var target in kvp.Value)
            {
                statsFileBuilder.AppendLine($"- {target}");
            }

            statsFileBuilder.AppendLine("---");
        }

        var reportFilePath = Path.Combine(_outputDir.FullName, $"{_timestamp}_failures.md");
        using var reportFileStream = File.Create(reportFilePath);
        using var reportFileWriter = new StreamWriter(reportFileStream);
        reportFileWriter.Write(statsFileBuilder.ToString());
    }
}
