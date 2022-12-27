using System;
using VSharp.Test;

namespace IntegrationTests
{
    [TestSvmFixture]
    public static class Bidirectional
    {
        private static void Nop() {}
        
        [TestSvm(100, strat: SearchStrategy.Backward)]
        public static int BackwardSmokeTest1(int i)
        {
            var j = i + 73;
            
            Nop();
            Nop();
            Nop();
            Nop();
            Nop();

            if (j > 100)
            {
                return 1;
            }

            return 0;
        }
        
        [TestSvm(100, strat: SearchStrategy.Backward)]
        public static int BackwardSmokeTest2(int i)
        {
            Nop();
            Nop();
            Nop();
            Nop();
            Nop();

            if (i + 73 > 100)
            {
                return 1;
            }

            return 0;
        }
        
        [TestSvm(100, strat: SearchStrategy.Backward)]
        public static int BackwardSmokeTest3(int i)
        {
            Nop();

            if (i > 100)
            {
                return 1;
            }

            return 0;
        }
    }
}
