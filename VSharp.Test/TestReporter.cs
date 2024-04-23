using System;
using System.Collections.Generic;
using System.IO;
using IntegrationTests;
using NUnit.Framework;
using VSharp.Explorer;
using VSharp.Interpreter.IL;

namespace VSharp.Test;

public readonly record struct TestInfo(
    string Name,
    UnitTest UnitTest,
    CilState.cilState State
);

internal class TestReporter : IReporter
{
    private readonly UnitTests _unitTests;
    private readonly TextWriter _logWriter;
    private List<TestInfo> _testInfos = new();

    public IReadOnlyList<TestInfo> States => _testInfos;

    public TestReporter(UnitTests unitTests, TextWriter logWriter)
    {
        _logWriter = logWriter;
        _unitTests = unitTests;
    }

    public void ReportFinished(UnitTest unitTest, CilState.cilState state)
    {
        var name = _unitTests.GenerateTest(unitTest);
        _testInfos.Add(new TestInfo(name, unitTest, state));
    }

    public void ReportException(UnitTest unitTest, CilState.cilState state)
    {
        var name = _unitTests.GenerateError(unitTest);
        _testInfos.Add(new TestInfo(name, unitTest, state));
    }

    public void ReportIIE(InsufficientInformationException iie) => _logWriter.WriteLine($"[IIE] {iie.Message}");
    public void ReportInternalFail(Method? method, Exception exn) => _logWriter.WriteLine($"[ERROR] {method.Name}: {exn}");
    public void ReportCrash(Exception exn) => _logWriter.WriteLine($"[CRASH] {exn}");
}
