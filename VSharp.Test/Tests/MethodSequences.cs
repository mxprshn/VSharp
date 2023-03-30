using System.Text;
using VSharp.Test;

namespace IntegrationTests;

public enum TestEnum
{
    Value1,
    Value2
}

public struct TestStruct
{
    public int Value { get; set; }
}

[TestSvmFixture]
public static class MethodSequences
{
    [TestSvm]
    public static bool Enums(TestEnum testEnum)
    {
        return testEnum switch
        {
            TestEnum.Value1 => true,
            TestEnum.Value2 => false
        };
    }

    [TestSvm]
    public static bool ByRef(ref int val)
    {
        if (val > 10)
        {
            return true;
        }

        return false;
    }

    [TestSvm]
    public static bool ByRef2(ref TestStruct val)
    {
        if (val.Value > 10)
        {
            return true;
        }

        return false;
    }

    [TestSvm]
    public static bool ByRef3(ref StringBuilder val)
    {
        if (val is not null)
        {
            return true;
        }

        return false;
    }

    [TestSvm]
    public static bool ByRef4(ref TestEnum val)
    {
        if (val == TestEnum.Value1)
        {
            return true;
        }

        return false;
    }

    [TestSvm]
    public static bool Classes(StringBuilder val)
    {
        if (val is not null)
        {
            return true;
        }

        return false;
    }

    [TestSvm]
    public static bool Structs(TestStruct val)
    {
        if (val.Value > 100)
        {
            return true;
        }

        return false;
    }
}
