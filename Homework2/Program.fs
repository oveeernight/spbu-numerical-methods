open System
open Microsoft.FSharp.Collections
open Homework2.Math

let func (x : double) : double = log (1.0 + x) - exp x
printfn "Enter points count"
let pointsCount = Console.ReadLine() |> int
printfn "Enter segment [a,b]"
let segment = Console.ReadLine().Split() |> Array.map double
let a,b = segment[0], segment[1]
let step = (b - a) / double pointsCount
let points =  [a..step..b]
let values = points |> Seq.map func
printfn "Enter x"
let x = Console.ReadLine() |> Double.Parse
printfn "Enter degree of interpolation polynomial, it's degree must be less than %i" pointsCount
let polyDegree = Console.ReadLine() |> int
if polyDegree >= pointsCount then
    raise (Exception($"Degree of interpolation polynomial must be less than %i{pointsCount}"))


let printTable name points values =
    printfn "%s:" name
    printf "%4s" "x"
    points |> Seq.iter (printf "|%9f")
    printfn ""
    printf "%4s" "f(x)"
    values |> Seq.iter (printf "|%9f")
    printfn ""
    
printTable "Base table" points values

let sortedPoints = getClosestPoints points x (polyDegree + 1)
let sortedValues = sortedPoints |> List.map func

printTable "Sorted by difference with x table" sortedPoints sortedValues

let lagrangePoly = getPolynomial LagrangeForm (sortedPoints, sortedValues)
let newtonPoly = getPolynomial NewtonForm (sortedPoints, sortedValues)

let printPolyTable xs poly (form : polynomialForm) =
    let values = xs |> List.map (fun x -> substituteInPoly x poly)
    printTable $"table in {form.ToString()}" xs values
  
// printPolyTable sortedPoints lagrangePoly LagrangeForm
// printPolyTable sortedPoints newtonPoly NewtonForm
   
printfn "Absolute error value for Lagrange form in %f is %17f" x (abs (func x - substituteInPoly x lagrangePoly))
printfn "Absolute error value for Newton form in %f is %17f" x (abs (func x - substituteInPoly x newtonPoly))
