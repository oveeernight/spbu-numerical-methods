module Homework2.Tests

open Common.Interpolation
open NUnit.Framework
open FsUnit

let precision = double 10e-10
let doublesAreEqual (x : double) y = abs(x - y) < precision

type testCaseSuit = {func : double -> double; segment: double * double; degree: int; pointsCount : int; x: double; form: polynomialForm}

let f1 (x : double) : double = log (1.0 + x) - exp x
let testCases = [{func = f1; segment = (1,2); degree = 11; pointsCount = 21;  x = 1.5; form = LagrangeForm}
                 {func = f1; segment = (1,2); degree = 11; pointsCount = 21;  x = 1.5; form = NewtonForm}]

[<TestCaseSource("testCases")>]
let ``Approximations Are Equal To Values In Interpolation Points`` testCase =
    let a, b = (fst testCase.segment, snd testCase.segment)
    let step = (b - a) / double testCase.pointsCount
    let points =  [a..step..b]
    let xs = getClosestPoints points testCase.x (testCase.degree + 1)
    let ys = List.map testCase.func xs
    let interpolationPoly = getPolynomial testCase.form (xs, ys)
    let zipped = List.zip xs ys
    Assert.True(List.forall (fun (x,y) -> doublesAreEqual (substituteInPoly x interpolationPoly) y) zipped)
    
    
[<Test>]
let `` Divided diffs calculated correctly`` =
    let points : double list = [-1;1;2;3]
    let arrayPoints = Array.ofList points
    let values : double list = [9;3;3;5]
    let dividedDiffs2 = kDividedDiffs arrayPoints values 1
    dividedDiffs2 |> should equivalent [-3; 0; 2]
    
    let dividedDiffs3 = kDividedDiffs arrayPoints dividedDiffs2 2
    dividedDiffs3 |> should equivalent [1; 1]
    
    let dividedDiffs4 = kDividedDiffs arrayPoints dividedDiffs3 3
    dividedDiffs4 |> should equivalent [0]    
