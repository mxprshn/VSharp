using Microsoft.Z3;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VSharp.Test
{

    [TestFixture]
    public class SimplificationTests
    {
        [Test]
        public void Simplify()
        {
            using var context = new Context();
            var simplifier = new Solver.Simplification.Simplifier(context);
        }
    }
}
