using Microsoft.Z3;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using VSharp.Solver;

namespace VSharp.Test
{

    [TestFixture]
    public class SimplificationTests
    {
        private Context c = null;
        private Simplification.Simplifier simplifier = null;

        [SetUp]
        public void SetUp()
        {
            c = new Context();
            simplifier = new Simplification.Simplifier(c);
        }

        [TearDown]
        public void TearDown()
        {
            c?.Dispose();
        }

        [Test]
        public void SimplifyTest1()
        {
            var op = c.MkIntConst("op");
            var y = c.MkIntConst("y");
            var expr = c.MkOr(
                c.MkEq(op, c.MkInt(0)),
                c.MkAnd(c.MkNot(c.MkEq(op, c.MkInt(0))), c.MkEq(op, c.MkInt(1))),
                c.MkAnd(c.MkNot(c.MkEq(op, c.MkInt(0))), c.MkNot(c.MkEq(op, c.MkInt(1))), c.MkEq(op, c.MkInt(2))),
                c.MkAnd(c.MkNot(c.MkEq(op, c.MkInt(0))), c.MkNot(c.MkEq(op, c.MkInt(1))), c.MkNot(c.MkEq(op, c.MkInt(2))), c.MkEq(op, c.MkInt(3)), c.MkNot(c.MkEq(y, c.MkInt(0)))),
                c.MkAnd(c.MkNot(c.MkEq(op, c.MkInt(0))), c.MkNot(c.MkEq(op, c.MkInt(1))), c.MkNot(c.MkEq(op, c.MkInt(2))), c.MkNot(c.MkEq(op, c.MkInt(3))))
            );
            Console.WriteLine(expr.ToString());
            var simplified = simplifier.Simplify(expr);
            Console.WriteLine(simplified.ToString());
            var expected = c.MkOr(
                c.MkNot(c.MkEq(y, c.MkInt(0))),
                c.MkNot(c.MkEq(op, c.MkInt(3)))
            );
            Assert.AreEqual(expected, simplified);
        }
        
        [Test]
        public void SimplifyTest2()
        {
            var x = c.MkIntConst("x");
            var expr = c.MkAnd(
                c.MkNot(c.MkEq(x, c.MkInt(1))),
                c.MkOr(
                    c.MkLe(x, c.MkInt(0)),
                    c.MkGt(x, c.MkInt(2)),
                    c.MkEq(x, c.MkInt(1))
                )
            );
            Console.WriteLine(expr.ToString());
            var simplified = simplifier.Simplify(expr);
            Console.WriteLine(simplified.ToString());
            var expected = c.MkOr(
                c.MkLe(x, c.MkInt(0)),
                c.MkGt(x, c.MkInt(2))
            );
            Assert.AreEqual(expected, simplified);
        }
    }
}
