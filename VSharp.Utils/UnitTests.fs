namespace VSharp

open System
open System.Collections.Generic
open System.IO
open VSharp

type UnitTests(outputDir : string, createSubdir : bool) =
    let testPrefix = "VSharp.tests."
    let testExtension = ".vst"
    let pathReplayExtension = ".vsr"
    let testFilePrefix = "test"
    let errorFilePrefix = "error"
    let mutable testNumber = 0u
    let mutable errorNumber = 0u
    let rootDir = Directory.CreateDirectory(if String.IsNullOrWhiteSpace outputDir then Directory.GetCurrentDirectory() else outputDir)
    let mutable currentDir = rootDir

    let () =
        if createSubdir then
            let testDirs = HashSet<string>()
            rootDir.EnumerateDirectories(testPrefix + "*") |> Seq.iter (fun dir -> dir.Name |> testDirs.Add |> ignore)
            let uniqueName = Seq.initInfinite id |> Seq.pick (fun i ->
                let name = testPrefix + i.ToString()
                if testDirs.Contains name then None else Some name)
            currentDir <- rootDir.CreateSubdirectory(uniqueName)
            let linkName = $"%s{rootDir.FullName}%c{Path.DirectorySeparatorChar}%s{testPrefix}last"
            FileSystem.createSymlink currentDir.FullName linkName
        else
            currentDir <- rootDir

        // TODO: rewrite better

        let tests = HashSet<string>()
        currentDir.EnumerateFiles("*" + testExtension) |> Seq.filter (fun f -> f.Name.StartsWith testFilePrefix) |> Seq.iter (fun dir -> dir.Name |> tests.Add |> ignore)
        testNumber <- Seq.initInfinite uint |> Seq.pick (fun i ->
            let name = testFilePrefix + (i + 1u).ToString() + testExtension
            if tests.Contains name then None else Some i)

        let errors = HashSet<string>()
        currentDir.EnumerateFiles("*" + testExtension) |> Seq.filter (fun f -> f.Name.StartsWith errorFilePrefix) |> Seq.iter (fun dir -> dir.Name |> errors.Add |> ignore)
        errorNumber <- Seq.initInfinite uint |> Seq.pick (fun i ->
            let name = errorFilePrefix + (i + 1u).ToString() + testExtension
            if errors.Contains name then None else Some i)

    let generateTest (test : UnitTest) (name : string) =
        test.Serialize $"%s{currentDir.FullName}%c{Path.DirectorySeparatorChar}%s{name}%s{testExtension}"

    let savePathReplay (test : UnitTest) (name : string) =
        match test.PathReplay with
        | Some pathReplay ->
            let path = Path.Combine(currentDir.FullName, $"{name}{pathReplayExtension}")
            pathReplay.Serialize path
        | None -> ()

    interface IDisposable with
        override x.Dispose() =
            ()

    member x.GenerateTest (test : UnitTest) =
        testNumber <- testNumber + 1u
        let name = testFilePrefix + testNumber.ToString()
        generateTest test name
        savePathReplay test name
        name

    member x.GenerateError (test : UnitTest) =
        errorNumber <- errorNumber + 1u
        let name = errorFilePrefix + errorNumber.ToString()
        generateTest test name
        savePathReplay test name
        name

    member x.WriteReport (reporter : Action<TextWriter>) =
        let reportFileName = $"%s{currentDir.FullName}%c{Path.DirectorySeparatorChar}report.txt"
        use stream = new FileStream(reportFileName, FileMode.OpenOrCreate, FileAccess.Write)
        use writer = new StreamWriter(stream)
        reporter.Invoke writer

    member x.TestDirectory with get() = currentDir

    member x.UnitTestsCount with get() = testNumber
    member x.ErrorsCount with get() = errorNumber
