module Homework4.Tests

open NUnit.Framework
open FsUnit
open Homework4

type testCase = {poly: polynomialBE; method: (double -> double) -> double -> double -> double}
let func0 x = polynomialBE.poly0.getSubstitutionValue x
let func1 x = polynomialBE.poly1.getSubstitutionValue x
let func2 x = polynomialBE.poly2.getSubstitutionValue x
let func3 x = polynomialBE.poly3.getSubstitutionValue x

let accuracyCorrespondence = [
                 {poly = polynomialBE.poly0; method = QuadratureFormulas.leftRectangle};
                 {poly = polynomialBE.poly0; method = QuadratureFormulas.rightRectangle}
                 {poly = polynomialBE.poly1; method = QuadratureFormulas.middleRectangle}
                 {poly = polynomialBE.poly1; method = QuadratureFormulas.trapezoid};
                 {poly = polynomialBE.poly3; method = QuadratureFormulas.simpson};
                 {poly = polynomialBE.poly3; method = QuadratureFormulas.frac38}]

[<TestCaseSource("accuracyCorrespondence")>]
let ``Algebraic degree of accuracy equals to expected`` testCase =
    let a, b = 0, 1
    let func x = testCase.poly.getSubstitutionValue x
    let methodResult = testCase.method func a b
    let tableInt = testCase.poly.definiteIntegral a b
    let doublesEqual x y =
        abs(x - y) < 10e-9
    Assert.True(doublesEqual methodResult tableInt)
let accuracyNonCorrespondence = [
                 {poly = polynomialBE.poly1; method = QuadratureFormulas.leftRectangle};
                 {poly = polynomialBE.poly1; method = QuadratureFormulas.rightRectangle}
                 {poly = polynomialBE.poly2; method = QuadratureFormulas.middleRectangle}
                 {poly = polynomialBE.poly2; method = QuadratureFormulas.trapezoid};
                 {poly = polynomialBE.poly4; method = QuadratureFormulas.simpson};
                 {poly = polynomialBE.poly4; method = QuadratureFormulas.frac38}]


[<TestCaseSource("accuracyNonCorrespondence")>]
let ``Algebraic degree of accuracy not higher then expected`` testCase =
    let a, b = 0, 1
    let func x = testCase.poly.getSubstitutionValue x
    let methodResult = testCase.method func a b
    let tableInt = testCase.poly.definiteIntegral a b
    let doublesEqual x y =
        abs(x - y) < 10e-9
    Assert.False(doublesEqual methodResult tableInt)