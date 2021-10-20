# V# Symbolic Execution Engine 

V# is the symbolic execution engine for .NET binaries, performing completely automated and unassisted test generation for .NET assemblies. It is cross-platform and supports .NET Core.

## How to build

Clone the repository and run `dotnet build --configuration Release`. After build finishes, run `dotnet publish  --configuration Release`.

## Testing a small function.

### 1. Using NUnit and V# API

Create an empty NUnit test project `DemoProject` and insert the following code:

```csharp
using System;
using NUnit.Framework;

namespace DemoProject
{

    public static class DemoClass
    {
        public static int Abs(int x)
        {
            int y = x;
            if (x < 0)
                y = -x;

            if (y < 0)
                throw new Exception("What?");

            return y;
        }
    }

    public class Tests
    {
        [Test]
        public void Test1()
        {
            var success = VSharp.TestGenerator.CoverAndRun(typeof(DemoClass));
            Assert.IsTrue(success);
        }

    }
}
```

Add reference to `VSharp.API.dll` assembly (`<V# build directory>/VSharp.API/bin/Release/net5.0/publish/VSharp.API.dll`). Set the Debug build type for `DemoProject` and place the break points into all `then` branches of conditions and `return` statement of `Abs`.

Run `Test1` in debug mode. If it throws `TypeInitializationException`, then also copy native Z3 library to destination directory of `DemoProject` (this will be fixed in future versions) and rerun the test. Native Z3 library can be found in `<V# build directory>/VSharp.API/bin/Release/net5.0/publish/runtimes/<platform>/native`.

The test will generate three unit tests for `Abs` function and run all three tests. You will sequentially see one non-negative input value, driving `Abs` directly to return, skipping the conditional branches, one value that gets into the `then` branch first of the first condition, and `INT_MIN` value which takes the `Abs` function throwing the exception.

Run the test coverage measurement tool to be sure the exhaustiveness of the generated test coverage. The generated tests can be found in `DemoProject` working directory, in `VSharp.tests.0` subfolder.

### 2. Using console runner

Create an empty class library project `DemoProject2` and insert the following code:

```csharp
namespace DemoProject2
{

    public class Customer
    {
        public int Id { get; set; }
        public string Name { get; set; }
        public string City { get; set; }

        public override bool Equals(Object other)
        {
            if (other is Customer otherCustomer)
            {
                return otherCustomer.Id == this.Id;
            }

            return false;
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(Id, Name, City);
        }
    }

    public class DemoClass2
    {
        private Customer _customer;

        public bool IsOurCustomer(Customer other)
        {
            if (other.Equals(_customer))
            {
                return true;
            }

            return false;
        }
    }

}
```

Compile this project. Go to `<V# build directory>/VSharp.Runner/bin/Release/net5.0/publish`. Run 
```bat
dotnet ./VSharp.Runner.dll --public-methods-of-class DemoProject2.DemoClass2 <path to DemoProject2.dll>
``` 

The engine will generate `*.vst` unit tests into the fresh directory `VSharp.tests.0`.

To run the generated tests, print 
```bat
dotnet VSharp.TestRunner.dll VSharp.tests.0
```

This command will run the `IsOurCustomer` function on different instances of `DemoClass2` and `other` parameter generated by symbolic execution engine. 

To measure the code coverage with `JetBrains.dotCover`, first install the coverage tool by running `dotnet tool install JetBrains.dotCover.GlobalTool`. Now you can generage the test coverage report by running 
```bat
dotnet dotcover --dcFilters="-:module=Microsoft.*;-:module=FSharp.*;-:class=VSharp.*;-:module=VSharp.Utils" VSharp.TestRunner.dll VSharp.tests.0 --dcReportType=DetailedXML
```

The coverage report will be generated into the `dotCover.Output.xml` file. Enjoy the exhaustive test coverage!

### 3. Further exploration

Explore our [integration testing programs](https://github.com/VSharp-team/VSharp/tree/master/VSharp.Test/Tests) codebase. Try V# on your programs!

## Current state

The project is currently in active development stage and yet unreleased. If you encounter the problem, consider [submitting the issue](https://github.com/VSharp-team/VSharp/issues).

## License

The project is licensed under the [Apache License Version 2.0](https://www.apache.org/licenses/LICENSE-2.0)
