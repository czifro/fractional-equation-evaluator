# fractional-equation-evaluator

This is for a coding challenge

## System Requirements

- .NET Core: 2.1.403

or

- Docker: latest

## TL;DR

Using .NET Core:

- `sh tools/install.sh`
- `.paket/paket install`
- `.fake-cli/fake build`
- `dotnet run src/FractionalEquationEvaluator.CliRunner/FractionalEquationEvaluator.CliRunner.fsproj`

Using Docker:

- `sh scripts/docker-build.sh`
- `sh scripts/docker-run.sh`

## Coding Challenge

Write a command line program in the language of your choice that will take operations on fractions as an input and produce a fractional result.

- Legal operators shall be *, /, +, - (multiply, divide, add, subtract)
- Operands and operators shall be separated by one or more spaces
- Mixed numbers will be represented by whole_numerator/denominator. e.g. "3_1/4"
- Improper fractions and whole numbers are also allowed as operands

Example run:
? 1/2 * 3_3/4
= 1_7/8

? 2_3/8 + 9/8
= 3_1/2

## Using .NET Core

This project was built with .NET Core 2.1.403. The `dotnet` toolchain is used to build and run the project. Additional tools like Paket and FAKE are installed locally using the `dotnet` toolchain. The helper script `tools/install.sh` will run the appropriate commands to install the helper tools. Paket is used to manage the dependencies used in this project. See the [Paket website](https://fsprojects.github.io/Paket/) for more details. FAKE is used to help automate steps in generating a build. See the [FAKE website](https://fake.build/) for more details. Once the tools are installed, use `.paket/paket install` to install the project's dependencies. After that, use `.fake-cli/fake build` to compile the project and run the unit tests. Once the project is compiled and all tests pass, use `dotnet run src/FractionalEquationEvaluator.CliRunner/FractionalEquationEvaluator.CliRunner.fsproj` to run the CLI runner for the project.

## Using Docker

There is support for Docker so that the steps outlined for using .NET Core can be contained within a Docker container. Running `sh scripts/docker-build.sh` will create a Docker image that contains the source, tests, restored dependencies, and a tested source build. Once the image is built, running `sh scripts/docker-run.sh` will start an interactive container running the program. The script `scripts/docker-rm.sh` has been provided to clean up the Docker image.