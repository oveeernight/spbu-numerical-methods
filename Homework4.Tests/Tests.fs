module Homework4.Tests

open NUnit.Framework
open FsUnit
open Homework4

type testCase = {poly: polynomialBE; method: (double -> double) -> double -> double -> double}

let accuracyCorrespondence = [
                 {poly = polynomialBE.poly0; method = Quadratures.leftRectangle};
                 {poly = polynomialBE.poly0; method = Quadratures.rightRectangle}
                 {poly = polynomialBE.poly1; method = Quadratures.middleRectangle}
                 {poly = polynomialBE.poly1; method = Quadratures.trapezoid};
                 {poly = polynomialBE.poly3; method = Quadratures.simpson};
                 {poly = polynomialBE.poly3; method = Quadratures.frac38}]

let doublesEqual x y =
    abs(x - y) < 10e-6

[<TestCaseSource("accuracyCorrespondence")>]
let ``Quadratures: algebraic degree of accuracy equals to expected`` testCase =
    let a, b = 0, 1
    let func x = testCase.poly.getSubstitutionValue x
    let methodResult = testCase.method func a b
    let tableInt = testCase.poly.definiteIntegral a b
    Assert.True(doublesEqual methodResult tableInt)
let accuracyNonCorrespondence = [
                 {poly = polynomialBE.poly1; method = Quadratures.leftRectangle};
                 {poly = polynomialBE.poly1; method = Quadratures.rightRectangle}
                 {poly = polynomialBE.poly2; method = Quadratures.middleRectangle}
                 {poly = polynomialBE.poly2; method = Quadratures.trapezoid};
                 {poly = polynomialBE.poly4; method = Quadratures.simpson};
                 {poly = polynomialBE.poly4; method = Quadratures.frac38}]

[<TestCaseSource("accuracyNonCorrespondence")>]
let ``Quadratures: algebraic degree of accuracy not higher then expected`` testCase =
    let a, b = 0, 1
    let func x = testCase.poly.getSubstitutionValue x
    let methodResult = testCase.method func a b
    let tableInt = testCase.poly.definiteIntegral a b
    Assert.False(doublesEqual methodResult tableInt)
    
type compTestCase = {poly: polynomialBE; method: (double -> double) -> double -> double -> int -> double; segmentsCount: int}

let accuracyCorrespondenceComp = [
                 {poly = polynomialBE.poly0; method = CompositeQuadratures.leftRectangle; segmentsCount = 10};
                 {poly = polynomialBE.poly0; method = CompositeQuadratures.rightRectangle; segmentsCount = 10}
                 {poly = polynomialBE.poly1; method = CompositeQuadratures.middleRectangle; segmentsCount = 20}
                 {poly = polynomialBE.poly1; method = CompositeQuadratures.trapezoid; segmentsCount = 16};
                 {poly = polynomialBE.poly3; method = CompositeQuadratures.simpson; segmentsCount = 10}]

[<TestCaseSource("accuracyCorrespondenceComp")>]
let ``Composite Quadratures: algebraic degree of accuracy equals to expected`` testCase =
    let a, b = 0, 1
    let func x = testCase.poly.getSubstitutionValue x
    let methodResult = testCase.method func a b testCase.segmentsCount
    let tableInt = testCase.poly.definiteIntegral a b
    Assert.True(doublesEqual methodResult tableInt)