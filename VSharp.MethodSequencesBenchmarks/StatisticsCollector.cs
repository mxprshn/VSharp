using System.Text;
using System.Text.Json;
using MathNet.Numerics.Statistics;
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
        var bars = new List<Bar>();
        bars.Add(new Bar { Position = 1, Value = failedCount, FillColor = Colors.Red});
        ticks.Add(new Tick(1, "Возникла\nошибка"));
        bars.Add(new Bar { Position = 2, Value = timeoutedCount, FillColor = Colors.Yellow});
        ticks.Add(new Tick(2, "Лимит\nвремени\nисчепан"));
        var currentBar = 3;
        foreach (var seqLength in lengthToCount.Keys.Order())
        {
            bars.Add(new Bar { Position = currentBar, Value = lengthToCount[seqLength], FillColor = Colors.LawnGreen});
            ticks.Add(new Tick(currentBar, seqLength.ToString()));
            currentBar++;
        }

        var barPlot = plot.Add.Bars(bars);

        foreach (var bar in barPlot.Bars)
        {
            bar.Label = bar.Value.ToString();
        }

        barPlot.ValueLabelStyle.FontSize = 30;

        plot.Axes.Bottom.TickGenerator = new ScottPlot.TickGenerators.NumericManual(ticks.ToArray());
        plot.Axes.Bottom.MajorTickStyle.Length = 0;

        var tickGen = new ScottPlot.TickGenerators.NumericAutomatic();
        tickGen.TickDensity = 0.5;
        plot.Axes.Left.TickGenerator = tickGen;

        plot.HideGrid();

        plot.Axes.Margins(bottom: 0);
        plot.YLabel("Количество последовательностей", 30);
        plot.XLabel("Длина последовательности", 30);
        plot.Axes.Bottom.Label.OffsetY = 30;
        plot.Axes.Bottom.TickLabelStyle.FontSize = 30;
        plot.Axes.Left.TickLabelStyle.FontSize = 30;
        plot.Layout.Fixed(new PixelPadding(200));
        plot.Axes.Bottom.TickLabelStyle.OffsetY = 10;

        plot.SavePng(Path.Combine(_outputDir.FullName, $"{_timestamp}_histogram.png"), 2500, 1000);
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

    public void GetMaxSequenceGenerationTime()
    {
        var maxTime = 0u;
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

                if (stats.SequenceLength > 0u)
                {
                    maxTime = Math.Max(maxTime, stats.GenerationTimeMs);
                }
            }
        }
        Console.WriteLine($"Max sequence generation time: {maxTime}ms");
    }

    public void CountTotalStates()
    {
        var suiteToCount = new Dictionary<string, int>();
        foreach (var target in _benchmarkIds.Select(_targets.GetById))
        {
            var targetRootDirPath = Path.Combine(_runsDir.FullName, target.SuiteName, Utils.GetTargetDirName(target));
            var count = Directory.EnumerateFiles(targetRootDirPath, "*.vst").Count();
            if (suiteToCount.TryGetValue(target.SuiteName, out var currentCount))
            {
                suiteToCount[target.SuiteName] = currentCount + count;
            }
            else
            {
                suiteToCount[target.SuiteName] = count;
            }
        }

        foreach (var kvp in suiteToCount)
        {
            Console.WriteLine($"States in {kvp.Key}: {kvp.Value}");
        }
    }

    public void ComputeTimes()
    {
        var vsharpTimes = new List<float>();
        var overheadTimes = new List<float>();

        foreach (var target in _benchmarkIds.Select(_targets.GetById))
        {
            var targetRootDirPath = Path.Combine(_runsDir.FullName, target.SuiteName, Utils.GetTargetDirName(target));

            var methodStatsPath = Path.Combine(targetRootDirPath, "stats.json");
            if (!File.Exists(methodStatsPath))
            {
                continue;
            }

            var statsJson = File.ReadAllText(methodStatsPath);
            var methodStats = JsonSerializer.Deserialize<MethodStatistics>(statsJson);

            if (methodStats.GenerationTimeMillis <= 0)
            {
                continue;
            }

            vsharpTimes.Add(methodStats.GenerationTimeMillis);

            var lastRunDir = Directory.EnumerateDirectories(targetRootDirPath, "2024*").OrderDescending().FirstOrDefault();
            if (lastRunDir == null)
            {
                continue;
            }

            var cumulativeOverhead = 0f;

            foreach (var jsonFile in Directory.EnumerateFiles(lastRunDir, "*.json"))
            {
                var json = File.ReadAllText(jsonFile);
                var stats = JsonSerializer.Deserialize<SequenceStatistics>(json);

                var genTime = stats.GenerationTimeMs;
                if (string.IsNullOrEmpty(stats.Exception) && stats.SequenceLength == 0)
                {
                    genTime = 15000;
                }

                cumulativeOverhead += genTime;
            }

            overheadTimes.Add(cumulativeOverhead);

        }

        var vsharpMedian = vsharpTimes.Median();
        var seqsMedian = overheadTimes.Median();
        Console.WriteLine($"VSharp median: {vsharpMedian}");
        Console.WriteLine($"Seqs median: {seqsMedian}");
    }
}
