using System.Collections.Generic;
using System.IO;
using System.Linq;
using IntegrationTests;
using NotImplementedException = System.NotImplementedException;

namespace VSharp.Test.Benchmarks;

public class OpenRaTargets : BenchmarkTargets
{
    public OpenRaTargets(TextWriter logWriter, string benchmarksPath) :
        base(logWriter, benchmarksPath, "openra", "OpenRA.Game")
    {
    }



    // HotKey
        // IsValid
        // DisplayString

    public IEnumerable<BenchmarkTarget> HotKey() =>
        TargetsForMethods("HotKey",
            "IsValid", "DisplayString");

    // CellRegion
        // Contains(CellRegion region)
        // Contains(CPos cell)

    // MPos
        // Clamp
        // ToCPos(MapGridType gridType)


    // Order
        // Serialize

    // WAngle
        // Cos
        // Tan
        // WAngle.Lerp

    // Sheet
        // CreateBuffer
        // ReleaseBuffer
        // GetTexture

    // SheetBuilder
        // Add(byte[] src, SpriteFrameType type, Size size, float zRamp, in float3 spriteOffset, bool premultiplied = false)
        // Allocate(Size imageSize, float zRamp, in float3 spriteOffset, float scale = 1f)
        //

    // Method for CellRegion
    public IEnumerable<BenchmarkTarget> CellRegion() =>
        TargetsForMethods("CellRegion",
            "Contains");

    // Method for MPos
    public IEnumerable<BenchmarkTarget> MPos() =>
        TargetsForMethods("MPos",
            "Clamp", "ToCPos");

    // Method for Order
    public IEnumerable<BenchmarkTarget> Order() =>
        TargetsForMethods("Order",
            "Serialize");

    // Method for WAngle
    public IEnumerable<BenchmarkTarget> WAngle() =>
        TargetsForMethods("WAngle",
            "Cos", "Tan", "Lerp");

    // Method for Sheet
    public IEnumerable<BenchmarkTarget> Sheet() =>
        TargetsForMethods("Sheet",
            "CreateBuffer", "ReleaseBuffer", "GetTexture");

    // Method for SheetBuilder
    public IEnumerable<BenchmarkTarget> SheetBuilder() =>
        TargetsForMethods("SheetBuilder",
            "100666807",
            "100666812");

    public override IEnumerable<BenchmarkTarget> All()
    {
        return HotKey()
            .Concat(CellRegion())
            .Concat(MPos())
            .Concat(Order())
            .Concat(WAngle())
            .Concat(Sheet())
            .Concat(SheetBuilder());
    }
}
