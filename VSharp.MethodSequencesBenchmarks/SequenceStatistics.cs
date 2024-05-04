namespace VSharp.MethodSequencesBenchmarks;

public class SequenceStatistics
{
    public uint GenerationTimeMs { get; set; }
    public uint StepsCount { get; set; }
    public uint SequenceLength { get; set; }
    public string Exception { get; set; } = "";

    public bool IsUnsupported { get; set; } = false;
}
