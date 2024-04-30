using System;
using NUnit.Framework;
using VSharp.Explorer;

namespace VSharp.Test;

internal class TestReporter : IReporter
{
    private readonly UnitTests _unitTests;

    public TestReporter(UnitTests unitTests)
    {
        _unitTests = unitTests;
    }

    public void ReportFinished(UnitTest unitTest) => _unitTests.GenerateTest(unitTest);
    public void ReportException(UnitTest unitTest) => _unitTests.GenerateError(unitTest);
    public void ReportIIE(InsufficientInformationException iie) => TestContext.Progress.WriteLine($"[IIE] {iie.Message}");
    public void ReportInternalFail(Method? method, Exception exn) => TestContext.Progress.WriteLine($"[ERROR] {method.Name}: {exn}");
    public void ReportCrash(Exception exn) => TestContext.Progress.WriteLine($"[CRASH] {exn}");
}
