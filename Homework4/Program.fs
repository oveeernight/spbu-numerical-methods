open System
open Homework4

printfn "Choose task: \n options: \n-q (quadratures) \n-cq (composite quadratures) \n -cql (composite quadratures with l parameter \n"
let task = Console.ReadLine()
if task = "q" then do
    let func x : double = x * (sin (x * x))
    let integral x : double = - cos (x * x) / 2.0
    printfn "--------------Approximate calculation of the integral using quadrature formulas--------------"
    printfn "function: x * sin(x^2)"
    printfn "Enter segment [a,b]"
    let segment = Console.ReadLine().Split() |> Array.map double
    let a, b = segment[0], segment[1]
    let j = integral b - integral a
    let formulas = [Quadratures.leftRectangle; Quadratures.rightRectangle; Quadratures.middleRectangle; Quadratures.trapezoid; Quadratures.simpson; Quadratures.frac38]
    for f in formulas do
        let approxValue = f func a b
        printfn $"%A{f} absolute residual: %.9f{abs (approxValue - j)}"
else if task = "cq" then do
    let func x : double = x + sin x
    let integral x : double = x * x / 2.0 - cos x
    printfn "--------------Approximate calculation of the integral using quadrature formulas--------------"
    printfn "Function: x + sin x"
    printfn "Enter segment [a,b]"
    let segment = Console.ReadLine().Split() |> Array.map double
    let a, b = segment[0], segment[1]
    let j = integral b - integral a
    printfn $"Table integral value: %.9f{j}"
    printfn "Enter segments count to divide [a,b]"
    let m = Console.ReadLine() |> int
    let left = CompositeQuadratures.leftRectangle func a b
    let right = CompositeQuadratures.rightRectangle func a b
    let middle = CompositeQuadratures.middleRectangle func a b
    let trapezoid = CompositeQuadratures.trapezoid func a b
    let simpson = CompositeQuadratures.simpson func a b
    let formulas = [left; right; middle; trapezoid; simpson;]
    let errors = [CompositeQuadratures.leftRectTheoryError; CompositeQuadratures.rightRectTheoryError; CompositeQuadratures.middleRectTheoryError;
                  CompositeQuadratures.trapezoidTheoryError; CompositeQuadratures.simpsonTheoryError]
    for f, e in (List.zip formulas errors) do
        let approxValue = f m
        let theoryError = e a b m 2
        let error = approxValue - j
        printfn "~~~~~~~~~~~%A{f}~~~~~~~~~~~"
        printfn $"approximate integral value: {approxValue}"
        printfn $"%absolute residual: %.9f{abs error}"
        printfn $"difference with theory error: %.9f{abs (error - theoryError)}"
else if task = "cql" then do
    
    