namespace VSharp.MethodSequencesBenchmarks;

public class SequenceStatistics
{
    public uint GenerationTimeMs { get; set; }
    public uint StepsCount { get; set; }
    public uint SequenceLength { get; set; }
    public string CriticalException { get; set; } = "";

    public Dictionary<string, List<string>> ExplorationExceptions { get; set; }
    public bool IsUnsupported { get; set; } = false;
}
