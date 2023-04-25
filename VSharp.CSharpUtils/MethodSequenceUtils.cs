using System.Text;

namespace VSharp.CSharpUtils;

public static class MethodSequenceUtils
{
    public static void MethodSequenceBase()
    {

    }

    public static void SingleMethodCall(string kek, out StringBuilder result)
    {
        result = new StringBuilder(kek);
    }
}
