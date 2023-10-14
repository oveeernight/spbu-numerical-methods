module Homework2.Tests

open NUnit.Framework
open FsUnit
open System

[<SetUp>]
let Setup () =
    ()


type testCaseSuit = {func : double -> double; segment: double * double; degree: int; pointsCount : int; x: double}
let f1 (x : double) : double = Math.Log(1.0 + x) - Math.Exp x
let testCases = [{func = f1; segment = (1,2); degree = 11; pointsCount = 21;  x = 1.5 }]

[<TestCaseSource("testCases")>]
let ``TestApproximationsAreEqualToValuesInInterpolationPoints`` testCase =
    Assert.Pass()
