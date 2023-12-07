namespace Homework4

open System

module CompositeQuadratures =
    let leftRectangle (func : double -> double) a b m =
        let h = (b - a) / (double m)
        h * ([a..h..b] |> List.take m |> List.fold (fun acc x -> acc + func x) 0.0)

    let leftRectTheoryError (a : double) b (m : int) (derMax : double) : double =
        Math.Pow(b - a, 2.0) / (2.0 * double m) * derMax

    let rightRectangle (func : double -> double) a b m =
        let h = (b - a) / (double m)
        h * ([a..h..b] |> List.skip 1 |> List.fold (fun acc p -> acc + func p) 0.0)

    let rightRectTheoryError a b m derMax = leftRectTheoryError a b m derMax

    let middleRectangle (func : double -> double) a b m =
        let h = (b - a) / (double m)
        h * ([a..h..b] |> List.take m |> List.fold (fun acc p -> acc + func (p + 0.5 * h)) 0.0)
        
    let middleRectTheoryError (a : double) b (m : int) (secondDerMax : double) : double =
        Math.Pow(b - a, 3.0) / (24.0 * Math.Pow(m, 2)) * secondDerMax 

    let trapezoid (func : double -> double) a b m =
        let h = (b - a) / (double m)
        (h * 0.5) * ([a..h..b] |> List.mapi (fun i p -> if (i = 0 || i = m) then func p else 2.0 * (func p)) |> List.sum)
        
    let trapezoidTheoryError a b m secondDerMax : double = 2.0 * middleRectTheoryError a b m secondDerMax

    let simpson (func : double -> double) a b m =
        let h = (b - a) / (double m)
        let partition = List.init (m + 1) (fun i -> a + (double i * h))
        let doublePart = partition |> List.take m |> List.skip 1 |> List.fold (fun acc p -> acc + func p) 0.0
        let fourPart = partition |> List.take m |> List.fold (fun acc p -> acc + func (p + 0.5 * h)) 0.0
        (h / 6.0) * ((func (List.head partition)) + 2.0 * doublePart + 4.0 * fourPart + func (List.item m partition))

    let simpsonTheoryError (a : double) b (m : int) (fourthDerMax : double) : double =
        Math.Pow(b - a, 5.0) / (2880.0 * Math.Pow(m, 4.0)) * fourthDerMax
        
    let rungeRefinement jh jhl (l : int) (ast : int) =
        (Math.Pow(l, double (ast +  1)) * jhl - jh) / (Math.Pow(l, double (ast + 1)) - double 1)
