using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using Microsoft.Z3;
using NUnit.Framework;
using VSharp.CSharpUtils;
using VSharp.Explorer;
using VSharp.Interpreter.IL;
using VSharp.MethodSequences;
using VSharp.Test.Benchmarks;

namespace VSharp.Test.Tests;

class Reporter : IReporter
{
    private List<CilState.cilState> _states = new();
    public IReadOnlyCollection<CilState.cilState> States => _states;

    public void ReportFinished(UnitTest test, CilState.cilState state) { _states.Add(state); }

    public void ReportException(UnitTest test, CilState.cilState state) { _states.Add(state); }

    public void ReportIIE(InsufficientInformationException exception) { }

    public void ReportInternalFail(Method method, Exception exception) { }

    public void ReportCrash(Exception exception) { }
}

[TestFixture]
public class MethodSequenceTests
{
    [Test]
    public void SmokeTest()
    {
        var loanExamMethod = VSharpTargets.TaylorSwift().First();
        var searchStrategy = searchMode.NewInterleavedMode(searchMode.ExecutionTreeMode, 1, searchMode.ContributedCoverageMode, 1);
        var svmOptions = new SVMOptions(
            explorationMode: explorationMode.NewTestCoverageMode(coverageZone.MethodZone, searchStrategy),
            recThreshold: 1,
            solverTimeout: -1,
            visualize: false,
            releaseBranches: false,
            maxBufferSize: 128,
            prettyChars: true,
            checkAttributes: false,
            stopOnCoverageAchieved: -1,
            randomSeed: 11,
            stepsLimit: uint.MaxValue,
            savePathReplays: false
        );

        var explorationModeOptions = Explorer.explorationModeOptions.NewSVM(svmOptions);

        var explorationOptions = new ExplorationOptions(
            timeout: TimeSpan.FromSeconds(10),
            outputDirectory: new DirectoryInfo(TestContext.CurrentContext.WorkDirectory),
            explorationModeOptions: explorationModeOptions
        );

        var reporter = new Reporter();
        using var explorer = new Explorer.Explorer(explorationOptions, reporter);

        explorer.StartExploration(new[] {loanExamMethod.Method}, new Tuple<MethodBase, EntryPointConfiguration>[] { });

        var interestingStates =
            reporter.States.ToList().Where(SupportValidation.IsSupported);//Where(s => !s.IsUnhandledExceptionOrError).ToList();

        foreach (var state in interestingStates)
        {
            var stopwatch = new Stopwatch();
            stopwatch.Start();

            var newSearcher = new MethodSequenceSearcher(state);

            while (stopwatch.Elapsed < TimeSpan.FromSeconds(3.0))
            {
                var foundSequences = newSearcher.MakeStep();
                if (foundSequences.Count == 0)
                {
                    continue;
                }

                stopwatch.Stop();
                Console.WriteLine(stopwatch.Elapsed);
                Console.WriteLine(foundSequences[0].ToString());
                Console.WriteLine();

                break;
            }
        }
    }

    [Test]
    public void PowerShellTest()
    {
        var loanExamMethod = PowerShellTargets.EnablePSBreakpointCommand();//VSharpTargets.LoanExam().First();
        var searchStrategy = searchMode.NewInterleavedMode(searchMode.ExecutionTreeMode, 1, searchMode.ContributedCoverageMode, 1);

        Logger.configureWriter(Console.Out);

        var svmOptions = new SVMOptions(
            explorationMode: explorationMode.NewTestCoverageMode(coverageZone.MethodZone, searchStrategy),
            recThreshold: 1,
            solverTimeout: -1,
            visualize: false,
            releaseBranches: false,
            maxBufferSize: 128,
            prettyChars: true,
            checkAttributes: false,
            stopOnCoverageAchieved: -1,
            randomSeed: 11,
            stepsLimit: uint.MaxValue,
            savePathReplays: false
        );

        var explorationModeOptions = Explorer.explorationModeOptions.NewSVM(svmOptions);

        var explorationOptions = new ExplorationOptions(
            timeout: TimeSpan.FromSeconds(10),
            outputDirectory: new DirectoryInfo(TestContext.CurrentContext.WorkDirectory),
            explorationModeOptions: explorationModeOptions
        );

        var reporter = new Reporter();
        using var explorer = new Explorer.Explorer(explorationOptions, reporter);

        explorer.StartExploration(new[] {loanExamMethod.Method}, new Tuple<MethodBase, EntryPointConfiguration>[] { });

        var interestingStates =
            reporter.States.ToList();//Where(s => !s.IsUnhandledExceptionOrError).ToList();

        foreach (var state in interestingStates)
        {
            var stopwatch = new Stopwatch();
            stopwatch.Start();

            var newSearcher = new MethodSequenceSearcher(state);

            while (stopwatch.Elapsed < TimeSpan.FromSeconds(15.0))
            {
                var foundSequences = newSearcher.MakeStep();
                if (foundSequences.Count == 0)
                {
                    continue;
                }

                stopwatch.Stop();
                Console.WriteLine(stopwatch.Elapsed);
                Console.WriteLine(foundSequences[0].ToString());
                Console.WriteLine();

                break;
            }
        }
    }
}
