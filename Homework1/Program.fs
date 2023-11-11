open System
open Homework1.Math

let func (x : double) : double = Math.Sin x + Math.Pow(x, 3) - 9.0 * x + 3.0
let funcDer  (x : double) : double  = Math.Cos x + 3.0 * Math.Pow(x, 2.0) - 9.0
let eps : double = Math.Pow(10, -8)
printfn "-------------Quantitative methods for solving nonlinear equations-------------"
let a,b = (-5.0, 4.0)
printfn "Function: sin x + x^3 - 9x + 3"
printfn $"[A,B] = [%.1f{a},%.1f{b}]"
printfn $"Precision: %.8f{eps}"

let segments = findSegments func a b 1
printfn $"Segments with root count: %i{List.length segments}. segments: %A{segments}"

for segment in segments do
    printfn $"````````````````````Segment %A{segment}````````````````````"
    let x0 = fst segment
    let x1 = snd segment
    printfn $"First approximation of newton method %f{x0}"
    printfn $"First approximation of advanced newton method %f{x0}, %f{x1}"
    let bisectionMethodFixed = bisectionMethod func eps
    let newtonMethodFixed = newtonMethod func funcDer eps x0
    let newtonMethodAdvancedFixed = newtonMethodAdvanced func funcDer eps x0
    let secantMethodFixed = secantMethod func eps x0 x1
    let methods = [bisectionMethodFixed; newtonMethodFixed; newtonMethodAdvancedFixed; secantMethodFixed]
    let inspectMethod segment method =
        printfn"-------------------------------------------"
        let root, stepsCount, lastSegment = method segment
        printfn $"%A{method}"
        printfn $"Steps count: %i{stepsCount}, root: %.10f{root}"
        printfn $"Last segment: [%.12f{fst lastSegment}, %.12f{snd lastSegment}], it's length : %.12f{snd lastSegment - fst lastSegment}"
        printfn $"Residual abs: %.15f{Math.Abs (func root)}"
    List.iter (inspectMethod segment) methods
