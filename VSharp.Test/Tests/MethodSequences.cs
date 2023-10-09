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

public struct TestStruct3
{
    private int _value;

    public TestStruct3(int value)
    {
        _value = value;
    }

    public int GetValue() => _value;
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

public class ClassWithStructCtor
{
    private readonly int _value;
    private readonly TestStruct _testStruct;

    public ClassWithStructCtor(int value, TestStruct testStruct)
    {
        _value = value;
        _testStruct = testStruct;
    }

    public int GetTotalValue() => _value + _testStruct.Value;
}

public class ClassWithField1
{
    public int _value;
}

public class ClassWithField2
{
    public ClassWithField1 _value;
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

    // Получаются цепочки из нескольких одинаковых конструкторов сразу
    [TestSvm]
    public static bool ListSmokeTest(List<int> list)
    {
        if (list.Capacity > 5)
        {
            return true;
        }

        return false;
    }

    // ок ли, что если guided searcher запаузил стейт, а других стейтов нет, а время есть, то мы не
    // запускаем этот стейт снова
    [TestSvm(recThresholdForTest: 0)]
    public static void ListSmokeTest2(int i)
    {
        var list = new List<int>();
        list.Capacity = i;
    }

    [TestSvm]
    public static bool CtorWithStruct(ClassWithStructCtor obj)
    {
        if (obj.GetTotalValue() > 5)
        {
            return true;
        }

        return false;
    }

    [TestSvm]
    public static bool StructNonDefaultConstructor(TestStruct3 struct3)
    {
        if (struct3.GetValue() > 5)
        {
            return true;
        }

        return false;
    }

    [TestSvm]
    public static bool Decimals(decimal val)
    {
        if (val > 10_000_000)
        {
            return true;
        }

        return false;
    }

    [TestSvm]
    public static bool NewEffect(TestStruct str, int n, int m)
    {
        if (str.Value < 1)
        {
            return false;
        }

        var arr = new int[str.Value];

        if (arr[m] + n > 2)
        {
            return true;
        }

        return false;
    }

    [TestSvm]
    public static int ByRefWithObject(ref int n, TestClass o)
    {
        var oValue = o.GetValue();
        if (n > 10 && oValue > 10 && n + oValue == 100)
        {
            return 0;
        }

        return 1;
    }

    [TestSvm]
    public static int Arrays1(int[] arr)
    {
        for (var i = 0; i < 5; i++)
        {
            if (arr[i] != i)
            {
                return 0;
            }
        }

        return 1;
    }

    [TestSvm]
    public static int Arrays2(StringBuilder[] arr)
    {
        for (var i = 0; i < 5; i++)
        {
            if (arr[i] == null)
            {
                return 0;
            }
        }

        return 1;
    }

    [TestSvm]
    public static int StringAndClass(string str, TestClass cls)
    {
        if (cls.GetValue() > 5)
        {
            if ("str" == str)
            {
                return 1;
            }

            return 2;
        }

        return 3;
    }

    [TestSvm]
    public static int IntAndClass(int i, TestClass cls)
    {
        if (cls.GetValue() > 5)
        {
            if (i == cls.GetValue())
            {
                return 1;
            }

            return 2;
        }

        return 1;
    }

    [TestSvm]
    public static int IntArrayAndClass(int[] a, TestClass cls)
    {
        if (cls.GetValue() > 5)
        {
            if (a[5] == cls.GetValue())
            {
                return 3;
            }

            return 2;
        }

        return 1;
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

[TestSvmFixture]
public struct TestStruct2
{
    [TestSvm]
    public bool StructThisTest(TestClass testClass)
    {
        if (testClass.GetValue() > 55)
        {
            return true;
        }

        return false;
    }
}
