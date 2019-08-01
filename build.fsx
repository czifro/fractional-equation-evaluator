// FAKE build script
// --------------------------------------------------------------------------------------
#r "paket: groupref build //"
#load ".fake/build.fsx/intellisense.fsx"

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

module Trace = Fake.Core.Trace

[<AutoOpen>]
module Helpers =

  [<RequireQualifiedAccess>]
  module String =

    let contains pattern (src:string) = src.Contains pattern

  let (<|>) f g x = f x || g x
  let (</>) = Fake.IO.Path.combine
  let DoNothing = ignore

[<RequireQualifiedAccess>]
module Repository =

  open System.IO
  open Fake.Core
  open Fake.Tools

  let rootDirectory =
    Trace.log "Loading repo root directory info"
    let rec findRepoRoot (currentDir:DirectoryInfo) : DirectoryInfo =
      let gitDir = currentDir.GetDirectories() |> Seq.tryFind(fun dir -> dir.Name = ".git")
      match gitDir with
      | Some _ -> currentDir
      | None ->
        if currentDir.FullName = "/" then failwith "Couldn't find .git directory"
        else findRepoRoot currentDir.Parent

    System.Environment.CurrentDirectory
    |> DirectoryInfo
    |> findRepoRoot

[<RequireQualifiedAccess>]
module Project =

  open System.IO
  module String = Fake.Core.String

  let private isCsproj (file:FileInfo) = file.Extension = ".csproj"
  let private isFsproj (file:FileInfo) = file.Extension = ".fsproj"

  let private loadDotNetProjects (dir:DirectoryInfo) =
    Trace.logfn "Loading .NET projects in %s" dir.Name
    dir.GetDirectories()
    |> Seq.collect(fun dir ->
      dir.GetFiles() |> Seq.filter (isCsproj <|> isFsproj)
    )

  let private getProjectFileRelativePath (file:FileInfo) =
    Trace.logfn "Getting relative path for %s" file.Name
    let repoRoot = Repository.rootDirectory
    let rec buildRelativePath (dir:DirectoryInfo) =
      if dir.FullName = repoRoot.FullName then ""
      else (buildRelativePath dir.Parent) </> dir.Name
    (buildRelativePath file.Directory) </> file.Name

  let projects =
    Trace.log "Loading src projects"
    let projectsDir = "src"
    projectsDir
    |> DirectoryInfo
    |> loadDotNetProjects
    |> Seq.map getProjectFileRelativePath
    |> Seq.toList

  let mainProject =
    projects
    |> List.find (String.contains "FractionalEquationEvaluator.CliRunner")

  let tests =
    Trace.log "Loading test projects"
    let testsDir = "tests"
    testsDir
    |> DirectoryInfo
    |> (fun dir -> if dir.Exists then Some dir else None)
    |> Option.map (
      loadDotNetProjects
      >> Seq.map getProjectFileRelativePath
      >> Seq.toList
    )
    |> Option.defaultValue List.empty

  let unitTests =
    tests
    |> List.filter ((String.contains "UnitTests") <|> (String.contains "FunctionalTests"))

  // We don't have these types of test, so we can comment this out for now
  // let integrationTests = tests |> List.filter (String.contains "IntegrationTests")

[<RequireQualifiedAccess>]
module Build =

  let buildSolution _ =
    Fake.DotNet.DotNet.build(fun options ->
      { options with Configuration = Fake.DotNet.DotNet.BuildConfiguration.Debug }
    ) ""

[<RequireQualifiedAccess>]
module Test =

  open Fake.DotNet

  type TestSuiteSelection =
    | UnitTestsOnly
    | IntegrationTestsOnly
    | FullTestSuite

  let private unitTest _ =
    Trace.log "Running unit tests"
    let runTest proj =
      Trace.logfn "Unit testing project: %s" proj
      DotNet.test(fun options ->
        { options with NoBuild = true }
      ) proj
    Project.unitTests |> List.iter runTest

  let private integrationTest _ = ()
    // We don't need this right now, so commenting this out for now
    // Trace.log "Running integration tests"
    // let runTest proj =
    //   Trace.logfn "Integration testing project: %s" proj
    //   DotNet.test(fun options ->
    //     { options with NoBuild = true }
    //   ) proj
    // Project.integrationTests |> List.iter runTest

  let runTestSuite =
    function
    | UnitTestsOnly -> Trace.log "Test Suite Selection: Unit Tests Only"; unitTest
    | IntegrationTestsOnly -> Trace.log "Test Suite Selection: Integration Tests Only"; integrationTest
    | FullTestSuite -> Trace.log "Test Suite Selection: Full Test Suite"; unitTest >> integrationTest

// --------------------------------------------------------------------------------------
// Build Targets
// --------------------------------------------------------------------------------------
module Target = Fake.Core.Target

Target.create "Build" Build.buildSolution

type TSS = Test.TestSuiteSelection

Target.create "Unit-Test"
  (Test.runTestSuite TSS.UnitTestsOnly)

Target.create "All" DoNothing

// --------------------------------------------------------------------------------------
// Build order
// --------------------------------------------------------------------------------------
open Fake.Core.TargetOperators

"Build"
  ==> "Unit-Test"
  ==> "All"

Target.runOrDefault "All"