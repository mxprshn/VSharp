#nullable enable
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using VSharp.CSharpUtils;
using VSharp.Interpreter.IL;

namespace VSharp
{
    /// <summary>
    /// Summary of V# test generation process.
    /// </summary>
    public sealed class Statistics
    {
        internal Statistics(
            TimeSpan time,
            DirectoryInfo outputDir,
            uint testsCount,
            uint errorsCount,
            uint stepsCount,
            IEnumerable<string> iies,
            IEnumerable<GeneratedTestInfo> generatedTestInfos)
        {
            TestGenerationTime = time;
            OutputDir = outputDir;
            TestsCount = testsCount;
            ErrorsCount = errorsCount;
            StepsCount = stepsCount;
            IncompleteBranches = iies;
            GeneratedTestInfos = generatedTestInfos;
        }

        /// <summary>
        /// Overall time of test generation.
        /// </summary>
        public TimeSpan TestGenerationTime { get; }

        /// <summary>
        /// Directory where *.vst tests are placed
        /// </summary>
        public DirectoryInfo OutputDir { get; }

        /// <summary>
        /// The amount of generated unit tests.
        /// </summary>
        public uint TestsCount { get; }

        /// <summary>
        /// The amount of errors found.
        /// </summary>
        public uint ErrorsCount { get; }

        /// <summary>
        /// The amount of symbolic machine steps.
        /// </summary>
        public uint StepsCount { get; }

        /// <summary>
        /// Some program branches might be failed to investigate. This enumerates the reasons of such failures.
        /// </summary>
        public IEnumerable<string> IncompleteBranches { get; }

        /// <summary>
        /// Generated tests statistics.
        /// </summary>
        public IEnumerable<GeneratedTestInfo> GeneratedTestInfos { get; }

        /// <summary>
        /// Writes textual summary of test generation process.
        /// </summary>
        /// <param name="writer">Output writer.</param>
        // TODO: Unify with Statistics.PrintStatistics
        public void GenerateReport(TextWriter writer)
        {
            writer.WriteLine("Total time: {0:00}:{1:00}:{2:00}.{3}.", TestGenerationTime.Hours,
                TestGenerationTime.Minutes, TestGenerationTime.Seconds, TestGenerationTime.Milliseconds);
            writer.WriteLine("Total coverage: {0}", GeneratedTestInfos.LastOrDefault().Coverage);
            var count = IncompleteBranches.Count();
            if (count > 0)
            {
                writer.WriteLine();
                writer.WriteLine($"{count} branch(es) with insufficient input information!");
                foreach (var message in IncompleteBranches)
                {
                    writer.WriteLine(message);
                }
            }
            writer.WriteLine($"Test results written to {OutputDir.FullName}");
            writer.WriteLine($"Tests generated: {TestsCount}");
            writer.WriteLine($"Errors generated: {ErrorsCount}");
        }

        /// <summary>
        /// Gets all generated '.vst' files
        /// </summary>
        public IEnumerable<FileInfo> Results()
        {
            return OutputDir.GetFiles("*.vst");
        }
    }

    public static class TestGenerator
    {
        private static Statistics StartExploration(
            IEnumerable<MethodBase> methods,
            coverageZone coverageZone,
            VSharpOptions options,
            string[]? mainArguments = null)
        {
            Logger.currentLogLevel = options.Verbosity.ToLoggerLevel();

            var unitTests = new UnitTests(options.OutputDirectory);
            var baseSearchMode = options.SearchStrategy.ToSiliMode();
            // TODO: customize search strategies via console options
            var siliOptions =
                new SiliOptions(
                    explorationMode: explorationMode.NewTestCoverageMode(
                        coverageZone,
                        options.Timeout > 0 ? searchMode.NewFairMode(baseSearchMode) : baseSearchMode
                    ),
                    outputDirectory: unitTests.TestDirectory,
                    recThreshold: options.RecursionThreshold,
                    timeout: options.Timeout,
                    solverTimeout: options.SolverTimeout,
                    visualize: false,
                    releaseBranches: options.ReleaseBranches,
                    maxBufferSize: 128,
                    checkAttributes: true,
                    stopOnCoverageAchieved: 100,
                    randomSeed: options.RandomSeed,
                    stepsLimit: options.StepsLimit
                );

            using var explorer = new SILI(siliOptions);
            var statistics = explorer.Statistics;

            void HandleInternalFail(Method? method, Exception exception)
            {
                if (options.Verbosity == Verbosity.Quiet)
                {
                    return;
                }

                if (exception is UnknownMethodException unknownMethodException)
                {
                    Logger.printLogString(Logger.Error, $"Unknown method: {unknownMethodException.Method.FullName}");
                    return;
                }

                var messageBuilder = new StringBuilder();

                if (method is not null)
                {
                    messageBuilder.AppendLine($"Explored method: {method.DeclaringType}.{method.Name}");
                }

                messageBuilder.Append($"Exception: {exception.GetType()} {exception.Message}");

                var trace = new StackTrace(exception, true);
                var frame = trace.GetFrame(0);

                if (frame is not null)
                {
                    messageBuilder.AppendLine();
                    messageBuilder.Append($"At: {frame.GetFileName()}, {frame.GetFileLineNumber()}");
                }

                Logger.printLogString(Logger.Error, messageBuilder.ToString());
            }

            void HandleCrash(Exception exception)
            {
                if (options.Verbosity == Verbosity.Quiet)
                {
                    return;
                }

                Logger.printLogString(Logger.Critical, $"{exception}");
            }

            var isolated = new List<MethodBase>();
            var entryPoints = new List<Tuple<MethodBase, string[]?>>();

            foreach (var method in methods)
            {
                var normalizedMethod = AssemblyManager.NormalizeMethod(method);
                if (normalizedMethod == normalizedMethod.Module.Assembly.EntryPoint)
                {
                    entryPoints.Add(new Tuple<MethodBase, string[]?>(normalizedMethod, mainArguments));
                }
                else
                {
                    isolated.Add(normalizedMethod);
                }
            }

            explorer.Interpret(isolated, entryPoints, unitTests.GenerateTest, unitTests.GenerateError, _ => { },
                HandleInternalFail, HandleCrash);

            var generatedTestInfos = statistics.GeneratedTestInfos.Select(i =>
                new GeneratedTestInfo(
                    IsError: i.isError,
                    ExecutionTime: i.executionTime,
                    StepsCount: i.stepsCount,
                    Coverage: i.coverage
                )
            );

            var result = new Statistics(
                statistics.CurrentExplorationTime,
                unitTests.TestDirectory,
                unitTests.UnitTestsCount,
                unitTests.ErrorsCount,
                statistics.StepsCount,
                statistics.IncompleteStates.Select(e => e.iie.Value.Message).Distinct(),
                generatedTestInfos);
            unitTests.WriteReport(statistics.PrintStatistics);

            return result;
        }

        private static void Render(Statistics statistics, Type? declaringType = null, bool singleFile = false, DirectoryInfo? outputDir = null)
        {
            TestRenderer.Renderer.Render(
                statistics.Results(),
                false,
                singleFile,
                declaringType,
                // Getting parent directory of tests: "VSharp.tests.*/.."
                outputDir ?? statistics.OutputDir.Parent
            );
        }

        private static searchMode ToSiliMode(this SearchStrategy searchStrategy)
        {
            return searchStrategy switch
            {
                SearchStrategy.BFS => searchMode.BFSMode,
                SearchStrategy.DFS => searchMode.DFSMode,
                SearchStrategy.ShortestDistance => searchMode.ShortestDistanceBasedMode,
                SearchStrategy.RandomShortestDistance => searchMode.RandomShortestDistanceBasedMode,
                SearchStrategy.ContributedCoverage => searchMode.ContributedCoverageMode,
                SearchStrategy.ExecutionTree => searchMode.ExecutionTreeMode,
                SearchStrategy.ExecutionTreeContributedCoverage => searchMode.NewInterleavedMode(searchMode.ExecutionTreeMode, 1, searchMode.ContributedCoverageMode, 1),
                SearchStrategy.Interleaved => searchMode.NewInterleavedMode(searchMode.ShortestDistanceBasedMode, 1, searchMode.ContributedCoverageMode, 9),
                _ => throw new UnreachableException("Unknown search strategy")
            };
        }

        private static int ToLoggerLevel(this Verbosity verbosity)
        {
            return verbosity switch
            {
                Verbosity.Quiet => Logger.Quiet,
                Verbosity.Critical => Logger.Critical,
                Verbosity.Error => Logger.Error,
                Verbosity.Warning => Logger.Warning,
                Verbosity.Info => Logger.Info,
                Verbosity.Trace => Logger.Trace,
                _ => throw new UnreachableException("Unknown verbosity level")
            };
        }

        public static bool Reproduce(DirectoryInfo testDir)
        {
            return TestRunner.TestRunner.ReproduceTests(testDir);
        }

        /// <summary>
        /// Generates test coverage for specified method.
        /// </summary>
        /// <param name="method">Method to be covered with tests.</param>
        /// <param name="options"><see cref="VSharpOptions"/>.</param>
        /// <returns>Summary of tests generation process.</returns>
        public static Statistics Cover(MethodBase method, VSharpOptions options = new())
        {
            AssemblyManager.LoadCopy(method.Module.Assembly);
            var methods = new List<MethodBase> {method};
            var statistics = StartExploration(methods, coverageZone.MethodZone, options);

            if (options.RenderTests)
                Render(statistics, method.DeclaringType, outputDir: options.RenderedTestsDirectoryInfo);

            return statistics;
        }

        /// <summary>
        /// Generates test coverage for specified methods.
        /// </summary>
        /// <param name="methods">Methods to be covered with tests.</param>
        /// <param name="options"><see cref="VSharpOptions"/>.</param>
        /// <returns>Summary of tests generation process.</returns>
        public static Statistics Cover(IEnumerable<MethodBase> methods, VSharpOptions options = new())
        {
            var methodArray = methods as MethodBase[] ?? methods.ToArray();
            var types = new HashSet<Type>();
            var assemblies = new HashSet<Assembly>();

            foreach (var method in methodArray)
            {
                assemblies.Add(method.Module.Assembly);

                if (method.DeclaringType is not null)
                {
                    types.Add(method.DeclaringType);
                }
            }

            foreach (var assembly in assemblies)
            {
                AssemblyManager.LoadCopy(assembly);
            }

            var zone = coverageZone.ModuleZone;

            if (methodArray.Length == 1)
            {
                zone = coverageZone.MethodZone;
            }
            else if (types.Count == 1)
            {
                zone = coverageZone.ClassZone;
            }

            var statistics = StartExploration(methodArray, zone, options);

            if (options.RenderTests)
                Render(statistics, types.SingleOrDefault(), outputDir: options.RenderedTestsDirectoryInfo);

            return statistics;
        }

        /// <summary>
        /// Generates test coverage for all public methods of specified type.
        /// </summary>
        /// <param name="type">Type to be covered with tests.</param>
        /// <param name="options"><see cref="VSharpOptions"/>.</param>
        /// <returns>Summary of tests generation process.</returns>
        /// <exception cref="ArgumentException">Thrown if specified class does not contain public methods.</exception>
        public static Statistics Cover(Type type, VSharpOptions options = new())
        {
            AssemblyManager.LoadCopy(type.Module.Assembly);

            var methods = new List<MethodBase>(type.EnumerateExplorableMethods());
            if (methods.Count == 0)
            {
                throw new ArgumentException("I've not found any public method or constructor of class " + type.FullName);
            }

            var statistics = StartExploration(methods, coverageZone.ClassZone, options);

            if (options.RenderTests)
                Render(statistics, type, outputDir: options.RenderedTestsDirectoryInfo);

            return statistics;
        }

        /// <summary>
        /// Generates test coverage for all public methods of specified types.
        /// </summary>
        /// <param name="types">Types to be covered with tests.</param>
        /// <param name="options"><see cref="VSharpOptions"/>.</param>
        /// <returns>Summary of tests generation process.</returns>
        /// <exception cref="ArgumentException">Thrown if specified classes don't contain public methods.</exception>
        public static Statistics Cover(IEnumerable<Type> types, VSharpOptions options = new())
        {
            var methods = new List<MethodBase>();
            var typesArray = types as Type[] ?? types.ToArray();
            var assemblies = new HashSet<Assembly>();

            foreach (var type in typesArray)
            {
                assemblies.Add(type.Assembly);
                methods.AddRange(type.EnumerateExplorableMethods());
            }

            if (methods.Count == 0)
            {
                var names = string.Join(", ", typesArray.Select(t => t.FullName));
                throw new ArgumentException("I've not found any public methods or constructors of classes " + names);
            }

            foreach (var assembly in assemblies)
            {
                AssemblyManager.LoadCopy(assembly);
            }

            var statistics = StartExploration(methods, coverageZone.ClassZone, options);

            if (options.RenderTests)
                Render(statistics, outputDir: options.RenderedTestsDirectoryInfo);

            return statistics;
        }

        /// <summary>
        /// Generates test coverage for all public methods of all public classes in the specified assembly.
        /// </summary>
        /// <param name="assembly">Assembly to be covered with tests.</param>
        /// <param name="options"><see cref="VSharpOptions"/>.</param>
        /// <param name="renderSingleFile">Forces tests generator to render all NUnit tests to one file.</param>
        /// <returns>Summary of tests generation process.</returns>
        /// <exception cref="ArgumentException">Thrown if no public methods found in assembly.
        /// </exception>
        public static Statistics Cover(Assembly assembly, VSharpOptions options = new(), bool renderSingleFile = false)
        {
            AssemblyManager.LoadCopy(assembly);
            var methods =
                new List<MethodBase>(assembly.EnumerateExplorableTypes().SelectMany(t => t.EnumerateExplorableMethods()));
            if (methods.Count == 0)
            {
                throw new ArgumentException("I've not found any public method in assembly");
            }

            var statistics = StartExploration(methods, coverageZone.ModuleZone, options);
            if (options.RenderTests)
                Render(statistics, singleFile: renderSingleFile, outputDir: options.RenderedTestsDirectoryInfo);
            return statistics;
        }

        /// <summary>
        /// Generates test coverage for the entry point of the specified assembly.
        /// </summary>
        /// <param name="assembly">Assembly to be covered with tests.</param>
        /// <param name="args">Command line arguments of entry point.</param>
        /// <param name="options"><see cref="VSharpOptions"/>.</param>
        /// <returns>Summary of tests generation process.</returns>
        /// <exception cref="ArgumentException">Thrown if assembly does not contain entry point.
        /// </exception>
        public static Statistics Cover(Assembly assembly, string[]? args, VSharpOptions options = new())
        {
            AssemblyManager.LoadCopy(assembly);

            var entryPoint = assembly.EntryPoint;
            if (entryPoint == null)
            {
                throw new ArgumentException("I've not found entry point in assembly");
            }

            var methods = new List<MethodBase> { entryPoint };

            var statistics = StartExploration(methods, coverageZone.MethodZone, options, args);
            if (options.RenderTests)
                Render(statistics);
            return statistics;
        }

        /// <summary>
        /// Generates test coverage for the specified method and runs all tests.
        /// </summary>
        /// <param name="method">Method to be covered with tests.</param>
        /// <param name="statistics">Summary of tests generation process.</param>
        /// <param name="options"><see cref="VSharpOptions"/>.</param>
        /// <returns>True if all generated tests have passed.</returns>
        public static bool CoverAndRun(MethodBase method, out Statistics statistics, VSharpOptions options = new())
        {
            statistics = Cover(method, options);
            return Reproduce(statistics.OutputDir);
        }

        /// <summary>
        /// Generates test coverage for the specified methods and runs all tests.
        /// </summary>
        /// <param name="methods">Methods to be covered with tests.</param>
        /// <param name="statistics">Summary of tests generation process.</param>
        /// <param name="options"><see cref="VSharpOptions"/>.</param>
        /// <returns>True if all generated tests have passed.</returns>
        public static bool CoverAndRun(IEnumerable<MethodBase> methods, out Statistics statistics, VSharpOptions options = new())
        {
            statistics = Cover(methods, options);
            return Reproduce(statistics.OutputDir);
        }

        /// <summary>
        /// Generates test coverage for the specified type and runs all tests.
        /// </summary>
        /// <param name="type">Type to be covered with tests.</param>
        /// <param name="statistics">Summary of tests generation process.</param>
        /// <param name="options"><see cref="VSharpOptions"/>.</param>
        /// <returns>True if all generated tests have passed.</returns>
        /// <exception cref="ArgumentException">Thrown if specified class does not contain public methods.</exception>
        public static bool CoverAndRun(Type type, out Statistics statistics, VSharpOptions options = new())
        {
            statistics = Cover(type, options);
            return Reproduce(statistics.OutputDir);
        }

        /// <summary>
        /// Generates test coverage for the specified types and runs all tests.
        /// </summary>
        /// <param name="types">Types to be covered with tests.</param>
        /// <param name="statistics">Summary of tests generation process.</param>
        /// <param name="options"><see cref="VSharpOptions"/>.</param>
        /// <returns>True if all generated tests have passed.</returns>
        /// <exception cref="ArgumentException">Thrown if specified classes don't contain public methods.</exception>
        public static bool CoverAndRun(IEnumerable<Type> types, out Statistics statistics, VSharpOptions options = new())
        {
            statistics = Cover(types, options);
            return Reproduce(statistics.OutputDir);
        }

        /// <summary>
        /// Generates test coverage for all public methods of all public classes of the specified assembly and runs all tests.
        /// </summary>
        /// <param name="assembly">Assembly to be covered with tests.</param>
        /// <param name="statistics">Summary of tests generation process.</param>
        /// <param name="options"><see cref="VSharpOptions"/>.</param>
        /// <returns>True if all generated tests have passed.</returns>
        /// <exception cref="ArgumentException">Thrown if no public methods found in assembly.
        /// </exception>
        public static bool CoverAndRun(Assembly assembly, out Statistics statistics, VSharpOptions options = new())
        {
            statistics = Cover(assembly, options);
            return Reproduce(statistics.OutputDir);
        }

        /// <summary>
        /// Generates test coverage for entry point of the specified assembly and runs all tests.
        /// </summary>
        /// <param name="assembly">Assembly to be covered with tests.</param>
        /// <param name="args">Command line arguments of entry point.</param>
        /// <param name="statistics">Summary of tests generation process.</param>
        /// <param name="options"><see cref="VSharpOptions"/>.</param>
        /// <returns>True if all generated tests have passed.</returns>
        /// <exception cref="ArgumentException">Thrown if assembly does not contain entry point.</exception>
        public static bool CoverAndRun(Assembly assembly, string[]? args, out Statistics statistics, VSharpOptions options = new())
        {
            statistics = Cover(assembly, args, options);
            return Reproduce(statistics.OutputDir);
        }
    }
}
