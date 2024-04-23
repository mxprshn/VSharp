#nullable enable
using System.Collections.Generic;
using VSharp.Explorer;
using VSharp.Interpreter.IL;

namespace VSharp.Test.Benchmarks;

public readonly record struct BenchmarkResult(
    bool IsSuccessful,
    SVMStatistics Statistics,
    UnitTests Tests,
    BenchmarkTarget Target,
    IReadOnlyList<TestInfo> TestInfos,
    bool? AllTestsPassed = null,
    bool? IsRenderingSuccessful = null,
    int? Coverage = null
);
