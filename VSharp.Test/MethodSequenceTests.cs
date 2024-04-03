using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
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

    public void ReportFinished(UnitTest test) { }

    public void ReportException(UnitTest missing_name) { }

    public void ReportIIE(InsufficientInformationException exception) { }

    public void ReportInternalFail(Method method, Exception exception) { }

    public void ReportCrash(Exception exception) { }

    public void ReportState(CilState.cilState state)
    {
        _states.Add(state);
    }
}

[TestFixture]
public class MethodSequenceTests
{
    [Test]
    public void SmokeTest()
    {
        var loanExamMethod = VSharpTargets.LoanExam().First();
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
            stepsLimit: uint.MaxValue
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
            reporter.States.Where(s => !s.IsUnhandledExceptionOrError).ToList();
        var state = interestingStates[2];

        var newSearcher = new MethodSequenceSearcher(state);
        
        for (;;)
        {
            var foundSequences = newSearcher.MakeStep();
            if (foundSequences.Count == 0)
            {
                continue;
            }
            
            Console.WriteLine(foundSequences[0].ToString());
            
            Assert.Pass();
        }
    }
}