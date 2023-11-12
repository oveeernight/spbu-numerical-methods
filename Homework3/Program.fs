open System
open Microsoft.FSharp.Collections
open Common.Interpolation

let func (x : double) : double = log (1.0 + x) - exp x
printfn "------------Reverse interpolation task, function = log(1+x) -e^x------------"
printfn "Enter values count"
let pointsCount = Console.ReadLine() |> int
printfn "Enter segment [a,b]"
let segment = Console.ReadLine().Split() |> Array.map double
let a,b = segment[0], segment[1]
let step = (b - a) / double pointsCount

let points =  [a..step..b]
let values = points |> Seq.map func

let printTable name points values =
    printfn "%s:" name
    printf "%4s" "x"
    points |> Seq.iter (printf "|%9f")
    printfn ""
    printf "%4s" "f(x)"
    values |> Seq.iter (printf "|%9f")
    printfn ""

printTable "start table" points values

printfn "Enter value for reverse interpolation"
let f = Console.ReadLine() |> Double.Parse
printfn "Enter degree of interpolation polynomial, it's degree must be less than %i" pointsCount
let polyDegree = Console.ReadLine() |> int
if polyDegree >= pointsCount then
    raise (Exception($"Degree of interpolation polynomial must be less than %i{pointsCount}"))
    
let sortedPoints = getClosestPoints points f (polyDegree + 1)
let sortedValues = sortedPoints |> List.map func

printTable "Sorted by difference with f table" sortedPoints sortedValues

let mutable newtonPoly = getPolynomial NewtonForm (sortedValues, sortedPoints)
let x = substituteInPoly f newtonPoly
printfn "~~~~interpolation for inverse function~~~~"

printfn "function has the value %.6f in point %.6f" f x

printfn "residual absolute value: %.9f" (abs ((func x) - f))


printfn "~~~~interpolation for base function and nonlinear equation search~~~~"

newtonPoly <- getPolynomial NewtonForm (sortedPoints, sortedValues)

let shiftedPoly x = (substituteInPoly x newtonPoly) - f
let rootSegments = Common.NonlinearEquations.findSegments shiftedPoly a b 1
let precision = 10e-9
let roots = List.map (Common.NonlinearEquations.bisectionMethod shiftedPoly precision) rootSegments
let printInfo searchInfo =
    let root, _, _ = searchInfo
    printfn "function has the value %.6f in point %.6f" f root
    printfn "residual absolute value: %.9f" (abs ((func root) - f))
List.iter printInfo roots