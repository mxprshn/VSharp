using VSharp.Test;

namespace IntegrationTests
{
    [TestSvmFixture]
    public static class Bidirectional
    {
        [TestSvm(strat: SearchStrategy.Backward)]
        public static int BackwardSmokeTest(int i)
        {
            var j = i + 73;

            if (j > 100)
            {
                return 1;
            }

            return 0;
        }
    }
}
