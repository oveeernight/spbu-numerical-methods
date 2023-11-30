open System
open Homework4
open QuadratureFormulas

printfn "--------------Approximate calculation of the integral using quadrature formulas--------------"
printfn "function: x * sin(x^2)"
printfn "Enter segment [a,b]"

let func x : double = x * (sin (x * x))
let integral a b : double = - (cos (b * b)) / 2.0 + (cos (a * a)) / 2.0

let segment = Console.ReadLine().Split() |> Array.map double
let a, b = segment[0], segment[1]
let j = integral a b
let leftRect = leftRectangle func
let rightRect = rightRectangle func
let middleRect = middleRectangle func
let trapezoid = trapezoid func
let simpson = simpson func
let frac38 = frac38 func
let formulas = [leftRect; rightRect; middleRect; trapezoid; simpson; frac38]
for f in formulas do
    let approxValue = f a b
    printfn $"{f.ToString()} absolute residual: %.9f{abs (approxValue - j)}"