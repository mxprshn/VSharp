using System.Collections.Generic;
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

public class TestClass
{
    private readonly int _value;

    public int AnotherValue { get; set; }
    public int YetAnotherValue { get; set; }

    public TestClass(int value)
    {
        _value = value;
    }

    public int GetValue() => _value;
}

public class TestClass2
{
    private readonly int _value;
    private readonly int _unusedValue;

    public TestClass2(int value, int unusedValue)
    {
        _value = value;
        _unusedValue = unusedValue;
    }

    public int GetValue() => _value;
}

public class TestClass3
{
    private readonly int _value;
    private readonly TestClass _testClass;

    public TestClass3(int value, TestClass testClass)
    {
        _value = value;
        _testClass = testClass;
    }

    public int GetTotalValue() => _value + _testClass.GetValue();
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

    [TestSvm]
    public static bool SimpleConstructor(TestClass cls)
    {
        if (cls.GetValue() > 10)
        {
            return true;
        }

        return false;
    }

    [TestSvm]
    public static bool ConstructorWithUnusedValue(TestClass2 cls)
    {
        if (cls.GetValue() > 10)
        {
            return true;
        }

        return false;
    }

    [TestSvm]
    public static bool SimpleSetter(TestClass cls)
    {
        if (cls.AnotherValue > 10)
        {
            return true;
        }

        return false;
    }

    [TestSvm]
    public static int TwoSetters(TestClass cls)
    {
        if (cls.AnotherValue > 10 && cls.YetAnotherValue > 15)
        {
            if (cls.AnotherValue + cls.YetAnotherValue == 100)
            {
                return 1;
            }

            return 2;
        }

        return 3;
    }

    [TestSvm]
    public static bool DependentConstructor(TestClass3 cls)
    {
        if (cls.GetTotalValue() > 100)
        {
            return true;
        }

        return false;
    }

    [TestSvm]
    public static bool MoreDependentConstructors(TestClass3 cls1, TestClass3 cls2)
    {
        if (cls1.GetTotalValue() + cls2.GetTotalValue() > 150)
        {
            return true;
        }

        return false;
    }

    [TestSvm]
    public static bool UnequalObjects(TestClass3 cls1, TestClass3 cls2)
    {
        if (cls1 != null && cls2 != null && cls1 != cls2 && cls1.GetTotalValue() + cls2.GetTotalValue() == 410)
        {
            return true;
        }

        return false;
    }

    [TestSvm]
    public static bool UnequalObjects1(TestClass3 cls1, TestClass3 cls2)
    {
        if (cls1 != null && cls2 != null && cls1 != cls2)
        {
            return true;
        }

        return false;
    }

    [TestSvm]
    public static bool ListSmokeTest(List<int> list)
    {
        if (list.Capacity > 5)
        {
            return true;
        }

        return false;
    }
}

[TestSvmFixture]
public class TestClass4
{
    public int Value { get; set; }

    [TestSvm]
    public bool InstanceMethod()
    {
        if (Value > 55)
        {
            return true;
        }

        return false;
    }
}
