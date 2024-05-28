using System.Reflection;
using VSharp.Test.Benchmarks;

namespace VSharp.MethodSequencesBenchmarks;

public class BenchmarkSelector
{
    private HashSet<Type> _primitivelyConstructedTypes = new();
    private HashSet<Type> _notPrimitivelyConstructedTypes = new();
    private BindingFlags Flags = BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public;

    private bool DeclaredInType(MethodBase method, Type type) => method.DeclaringType == type;

    private IEnumerable<MethodBase> EnumerateMethods(Type type)
    {
        return type.GetMethods(Flags).Where(m => m.GetMethodBody() != null && DeclaredInType(m, type));
    }

    private IEnumerable<MethodBase> EnumerateCtors(Type type)
    {
        return type.GetConstructors(Flags).Where(ctor => ctor.GetMethodBody() != null);
    }

    private bool IsPrimitivelyConstructed(Type typ, bool considerInterfaces)
    {
        if (considerInterfaces && (typ.IsInterface || typ.IsAbstract))
        {
            var concreteTypes = typ.Assembly.GetTypes()
                .Where(t => t.IsAssignableTo(typ) && t is { IsInterface: false, IsAbstract: false }).Take(10);
            return concreteTypes.Any(t => IsPrimitivelyConstructed(t, false));
        }

        if (typ.IsPrimitive || typ.IsEnum)
        {
            return true;
        }

        if (_primitivelyConstructedTypes.Contains(typ))
        {
            return true;
        }

        if (_notPrimitivelyConstructedTypes.Contains(typ))
        {
            return false;
        }

        var typeDef = typ;
        if (typ.IsGenericType)
        {
            typeDef = typ.GetGenericTypeDefinition();
        }

        if (typeDef.IsInterface || typeDef.IsAbstract)
        {
            _notPrimitivelyConstructedTypes.Add(typ);
            return false;
        }

        _primitivelyConstructedTypes.Add(typ);

        foreach (var ctor in EnumerateCtors(typ))
        {
            var isSuitableCtor = true;

            foreach (var ctorParameter in ctor.GetParameters())
            {
                if (!IsPrimitivelyConstructed(ctorParameter.ParameterType, false))
                {
                    isSuitableCtor = false;
                    break;
                }
            }

            if (isSuitableCtor)
            {
                return true;
            }
        }

        _primitivelyConstructedTypes.Remove(typ);
        _notPrimitivelyConstructedTypes.Add(typ);
        return false;
    }

    private static bool IsPublic(Type t)
    {
        return
            t.IsPublic
            || t is { IsNestedPublic: true, DeclaringType: not null } && IsPublic(t.DeclaringType);
    }

    private IEnumerable<Type> EnumerateExplorableTypes(Assembly assembly)
    {
        var types = new List<Type>();
        try
        {
            types.AddRange(assembly.GetTypes());
        }
        catch (ReflectionTypeLoadException e)
        {
            types.AddRange(e.Types.Where(type => type is not null)!);
        }

        return types.Where(IsPublic);
    }

    private static bool IsNotGetterOrSetter(MethodBase method)
    {
        return !method.Name.StartsWith("get_") && !method.Name.StartsWith("set_");
    }

    public IEnumerable<BenchmarkTarget> GetBenchmarkTargetsFromAssembly(Assembly assembly, string key)
    {
        foreach (var method in EnumerateExplorableTypes(assembly).Where(t => IsPrimitivelyConstructed(t, false))
                     .SelectMany(EnumerateMethods).Where(IsNotGetterOrSetter))
        {
            var isSuitableMethod = true;

            foreach (var ctorParameter in method.GetParameters())
            {
                if (!IsPrimitivelyConstructed(ctorParameter.ParameterType, true))
                {
                    isSuitableMethod = false;
                    break;
                }
            }

            if (isSuitableMethod)
            {
                yield return new BenchmarkTarget(method, key);
            }
        }
    }
}
