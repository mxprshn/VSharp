using VSharp.Test.Benchmarks;

namespace VSharp.MethodSequencesBenchmarks;

public class Utils
{
    public static string GetTargetDirName(BenchmarkTarget target)
    {
        return $"{target.Method.MetadataToken}_{target.Method.Name}";
    }
}
