open Microsoft.FSharp.Collections
open Common.Interpolation
open System
open Homework3

printfn "Choose task: \n options: \n-ip (reverse interpolation) \n-nd (numerical derivation)"
let task = Console.ReadLine()
if (task = "ip") then do
    let func (x : double) : double = log (1.0 + x) - exp x
    printfn "------------Reverse interpolation task, function = log(1+x) - e^x------------"
    let mutable exit = "ilovenumericalmethods"
    while (exit <> "exit") do
        printfn "Enter values count"
        let pointsCount = Console.ReadLine() |> int
        printfn "Enter segment [a,b]"
        let segment = Console.ReadLine().Split() |> Array.map double
        let a,b = segment[0], segment[1]
        let step = (b - a) / double pointsCount

        let points =  [a..step..b]
        let values = List.map func points

        Common.Printing.printFunctionTable "Base table" points values

        printfn "Enter value for reverse interpolation"
        let f = Console.ReadLine() |> double
        printfn "Enter degree of interpolation polynomial, it's degree must be less than %i" pointsCount
        let mutable polyDegree = Console.ReadLine() |> int
        while (polyDegree >= pointsCount) do
            printfn $"Degree of interpolation polynomial must be less than %i{pointsCount}. Enter correct value"
            polyDegree <- Console.ReadLine() |> int

        let sortedPoints = List.sortBy func points |> List.take (polyDegree + 1)
        let sortedValues = sortedPoints |> List.map func

        Common.Printing.printFunctionTable "Sorted by difference with f table" sortedPoints sortedValues

        let mutable newtonPoly = getPolynomial NewtonForm (sortedValues, sortedPoints)
        let x = substituteInPoly f newtonPoly
        printfn "~~~~~~interpolation for inverse function result~~~~~~"
        printfn "function has the value %.6f in point %.6f" f x
        printfn "residual absolute value: %.9f" (abs ((func x) - f))

        printfn "~~~~~~interpolation for base function and nonlinear equation search~~~~~~"

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
        printfn "print \"exit\" to exit program or \"continue\" to reset parameters"
        exit <- Console.ReadLine()
else if (task = "nd") then do
    let func (x : double) : double = exp (6.0 * x)
    let func' x = 6.0 * func x
    let func'' x = 36.0 * func x
    printfn "------------Numerical derivation task, function = e^(6x)------------"
    let mutable exit = "ilovemumericalmethods"
    while (exit <> "exit") do
        printfn "Enter values count"
        let pointsCount = Console.ReadLine() |> int
        printfn "Enter starting point a"
        let a = Console.ReadLine() |> double
        printfn "Enter step h"
        let h = Console.ReadLine() |> double
        let points = List.init pointsCount (fun i -> a + (double i) * h)
        let values = List.map func points
        Common.Printing.printFunctionTable "Base table" points values

        let printNumericalDerivationTable xs nds' nds'' =
            printfn "x_i                   /f(x_i)                /f'(x_i)_nd            /|f'(x_i)_t-f'(x_i)_nd| /f''(x_i)_nd         /|f''(x_i)_t-f''(x_i)_nd|   "
            let printRow x (nd',nd'') =
                printfn $"%22.6f{x}|%22.12f{func x}|%22.12f{nd'}|%22.12f{abs((func' x) - nd')}|%22.12f{nd''}|%22.12f{ abs((func'' x) - nd'')}"
            (List.zip nds' nds'') |> List.iter2 printRow xs
        let interval = Interval.fromList points
        let numericalDerivationValues' = Interval.calculateFirstDerivative func h interval
        let numericalDerivationValues'' = Interval.calculateSecondDerivative func h interval
        printNumericalDerivationTable points numericalDerivationValues' numericalDerivationValues''
        printfn "print \"exit\" to exit program or \"continue\" to reset parameters"
        exit <- Console.ReadLine()
else do
    printfn "unknown command"