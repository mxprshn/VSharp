﻿using System;
using System.Collections.Generic;
using NUnit.Framework;
using VSharp.Test.Tests.Typecast;

namespace VSharp.Test.Tests.Generic
{
    public interface IKeeper<in T>
    {
        void Keep(T obj);
    }

    public class Bag<T> : IKeeper<T>
    {
        private Queue<T> _queue;

        public Bag()
        {
            _queue = new Queue<T>();
        }

        public void Keep(T obj)
        {
            _queue.Enqueue(obj);
        }
    }

    [TestSvmFixture]
    public static class GenericInitialize<T, U, P, K, N, Z>
        where T : U
        where U : IKeeper<P>
        where P : struct, IKeeper<T>
        where K : class, IKeeper<U>
        where N : IKeeper<K>
        where Z : List<int>
    {
        [TestSvm]
        public static LinkedList<int> RetDictionary()
        {
            return new LinkedList<int>();
        }

        [TestSvm]
        public static List<double> RetList()
        {
            return new List<double>();
        }

        [Ignore("Need to delete Reference type constructor")]
        public static T RetT(T t)
        {
            return t;
        }
    }

    [TestSvmFixture]
    public static class GenericTest<T, U, P, K, N, Z>
        where T : U
        where U : IKeeper<P>
        where P : struct, IKeeper<T>
        where K : class, IKeeper<U>
        where N : IKeeper<K>
        where Z : List<int>
    {
        [Ignore("Need to delete Reference type constructor")]
        public static T RetT(T t)
        {
            return t;
        }

        [Ignore("Need to delete Reference type constructor")]
        public static U RetU(U u)
        {
            return u;
        }

        [TestSvm]
        public static P RetP(P p)
        {
            return p;
        }

        [TestSvm]
        public static K RetK(K k)
        {
            return k;
        }

        [Ignore("Need to delete Reference type constructor")]
        public static N RetT(N n)
        {
            return n;
        }

        [TestSvm]
        public static Z RetU(Z z)
        {
            return z;
        }
    }

    [TestSvmFixture]
    public static class GenericClass<R, V>
        where R : class
        where V : struct
    {
        [TestSvm]
        public static R RetR(R r)
        {
            return r;
        }

        [TestSvm]
        public static V RetV(V v)
        {
            return v;
        }
    }

    [TestSvmFixture]
    public static class SpecializeGenerics
    {
        [Ignore("Box is not implemented")]
        public static object RetConstructedRWithInt()
        {
            return GenericClass<object, int>.RetR(0);
        }

        [TestSvm]
        public static object RetConstructedRWithObject()
        {
            return GenericClass<object, int>.RetR(new object());
        }

        [TestSvm]
        public static int RetConstructedVWithInt()
        {
            return GenericClass<object, int>.RetV(0);
        }
    }

    [TestSvmFixture]
    public class Foo<T, U>
    {
        private T _filed;

        public Foo()
        {
        }

        [Ignore("Need to delete Reference type constructor")]
        public T GetFields()
        {
            return _filed;
        }

        [TestSvm]
        public void SetField(T f)
        {
            _filed = f;
        }
    }

    [TestSvmFixture]
    public static class GenericMethod
    {
        [TestSvm]
        public static int TestFoo(Foo<int, Piece> f)
        {
            if (f == null) return 0;
            return f.GetFields();
        }

        [TestSvm]
        public static int TestFoo(LinkedList<int> l)
        {
            if (l == null) return 0;
            if (l.First == null) return 1;
            return l.First.Value += 1;
        }
    }

    [TestSvmFixture]
    public static class TetsUnion
    {
//        public static Coord RetCoord(Object obj, Coord coord, int field)
//        {
//            if (obj is BlackPawn)
//            {
//                coord.X = 42;
//            }
//            if (obj is Pawn)
//            {
//                coord.X += 66;
//            }
//            return coord;
//        }

//        public static Object Ret(Object obj)
//        {
//            var f = obj as BlackPawn;
//            if (f != null)
//            {
//                f.SetNewField(42);
//            }
//            var a = obj as Pawn;
//            if (a != null)
//            {
//                int b = a.GetNewField();
//                a.SetNewField(b + 66);
//            }
//            return obj;
//        }

        [TestSvm]
        public static int RetWorked(Object obj, int a)
        {
            if (obj as BlackPawn != null)
            {
                a = 5;
            }
            if (obj as Pawn != null)
            {
                a = a + 6;
            }
            return a;
        }
    }
//    public static class GenericCast
//    {
//        public static void FilterAndKeep(List<Pawn> listPawn, IKeeper<Pawn> bag)
//        {
//            foreach (var pawn in listPawn)
//            {
//                if (pawn.GetNewField() > 5)
//                {
//                    bag.Keep(pawn);
//                }
//            }
//        }
//
//        public static void TestFilterAndKeep()
//        {
//            IKeeper<Piece> myKeeper = new Bag<Piece>();
//            List<Pawn> myList = new List<Pawn>();
//            FilterAndKeep(myList, myKeeper);
//        }
//
//        public static void TestContrvarianceAndCovariance()
//        {
//            Func<Piece, Pawn> func = (x => new Pawn(x.GetCoord(), x.GetRate()));
//            Func<Pawn, Piece> newFunc = func;
//        }
//    }


//    public interface IFooTest<T, U>
//        where T : IFoo<U>
//        where U : IFoo<T>
//    {
//
//    }
//
//    public class FooFoo<T, U> {}
//
    public interface IFoo<out T> where T : Typecast.Piece {}
//
//    public interface IFooOne : IFoo<IFooTwo> {}
//
//    public interface IFooTwo : IFoo<IFooOne> {}
//
//    public class RecFoo : Foo<RecFoo> {}
//    public class RecFooOne : Foo<RecFooTwo> {}
//    public class RecFooTwo : Foo<RecFooOne> {}
//
//    public class Ret<T, U>
//        where T : Foo<U>, new()
//        where U : Foo<T>
//    {
//TODO: don't parse, need fix
//        public delegate Object MyDelegateOne(String obj);
//
//        public delegate Object MyDelegateTwo(Object obj);
//
//        public T PropT { get; set; }
//        public U PropU { get; set; }
//
//        public bool TestCheckCast(RecFooOne obj1, RecFooTwo obj2)
//        {
//            return obj1 is T && obj2 is U;
//        }
//    }
}
