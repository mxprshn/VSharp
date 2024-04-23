namespace VSharp.MethodSequencesBenchmarks;

public class MethodStatistics
{
    public uint GenerationTimeMillis { get; set; }
    public int CoveragePercent { get; set; }
    public uint StepsCount { get; set; }
    public List<string> InternalFails { get; set; }
    public bool? AllTestsPassed { get; set; }
    public bool? IsRenderingSuccessful { get; set; }
}
