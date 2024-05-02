using System.Collections.Generic;
using System.IO;
using System.Linq;
using NotImplementedException = System.NotImplementedException;

namespace VSharp.Test.Benchmarks;

public class OsuTargets : BenchmarkTargets
{
    public OsuTargets(TextWriter logWriter, string benchmarksPath) :
        base(logWriter, benchmarksPath, "osu", "osu.Game")
    {
    }

    // BeatmapCarousel
        // SelectBeatmap
        // SelectNext
        // SelectNextRandom
        // Filter

    public IEnumerable<BenchmarkTarget> BeatmapCarousel() =>
        TargetsForMethods("BeatmapCarousel", "SelectBeatmap", "SelectNext", "SelectNextRandom", "Filter");

    // ScoreInfo
        // DeepClone
        // GetStatisticsForDisplay


    // RulesetInfo
        // CompareTo(RulesetInfo? other)
        // CreateInstance

    // HitObject
        // CreateSlidingSamples
        // CreateHitSampleInfo


    // DefaultJudgementPiece
        // PlayAnimation

    // ModColumn
        // SelectAll
        // DeselectAll
        // FlushPendingSelections

    // ModPresetTooltip
        // SetContent


    // Channel
        // AddLocalEcho
        // RemoveMessagesFromUser
        // ReplaceMessage

    // SoloScoreInfo
        // ShouldSerializeEndedAt
        // ShouldSerializeStartedAt
        // ShouldSerializeLegacyScoreId
        // ShouldSerializeLegacyTotalScore
        // ShouldSerializeMods
        // ShouldSerializeUserID
        // ShouldSerializeBeatmapID
        // ShouldSerializeBuildID

    // BeatmapInfo
        // AudioEquals
        // BackgroundEquals
        // ResetOnlineInfo

        // Method for ScoreInfo
    public IEnumerable<BenchmarkTarget> ScoreInfo() =>
        TargetsForMethods("ScoreInfo",
            "DeepClone", "GetStatisticsForDisplay");

    // Method for RulesetInfo
    public IEnumerable<BenchmarkTarget> RulesetInfo() =>
        TargetsForMethods("RulesetInfo",
            "100678064", "CreateInstance");

    // Method for HitObject
    public IEnumerable<BenchmarkTarget> HitObject() =>
        TargetsForMethods("HitObject",
            "CreateSlidingSamples", "CreateHitSampleInfo");

    // Method for DefaultJudgementPiece
    public IEnumerable<BenchmarkTarget> DefaultJudgementPiece() =>
        TargetsForMethods("DefaultJudgementPiece",
            "PlayAnimation");

    // Method for ModColumn
    public IEnumerable<BenchmarkTarget> ModColumn() =>
        TargetsForMethods("ModColumn",
            "SelectAll", "DeselectAll", "FlushPendingSelections");

    // Method for ModPresetTooltip
    public IEnumerable<BenchmarkTarget> ModPresetTooltip() =>
        TargetsForMethods("ModPresetTooltip",
            "SetContent");

    // Method for Channel
    public IEnumerable<BenchmarkTarget> Channel() =>
        TargetsForMethods("Channel",
            "AddLocalEcho", "RemoveMessagesFromUser", "ReplaceMessage");

    // Method for SoloScoreInfo
    public IEnumerable<BenchmarkTarget> SoloScoreInfo() =>
        TargetsForMethods("SoloScoreInfo",
            "ShouldSerializeEndedAt", "ShouldSerializeStartedAt",
            "ShouldSerializeLegacyScoreId", "ShouldSerializeLegacyTotalScore",
            "ShouldSerializeMods", "ShouldSerializeUserID",
            "ShouldSerializeBeatmapID", "ShouldSerializeBuildID");

    // Method for BeatmapInfo
    public IEnumerable<BenchmarkTarget> BeatmapInfo() =>
        TargetsForMethods("BeatmapInfo",
            "AudioEquals", "BackgroundEquals", "ResetOnlineInfo");

    public override IEnumerable<BenchmarkTarget> All()
    {
        return BeatmapCarousel().Concat(ScoreInfo())
            .Concat(RulesetInfo())
            .Concat(HitObject())
            .Concat(DefaultJudgementPiece())
            .Concat(ModColumn())
            .Concat(ModPresetTooltip())
            .Concat(Channel())
            .Concat(SoloScoreInfo())
            .Concat(BeatmapInfo());
    }
}
