namespace Homework2


module Math =
    type polynomial = (double -> double) list
    type polynomialForm =
    | LagrangeForm
    | NewtonForm    
    let private mul x y = double x * y
    let private sum x y = double x + y
    let private substituteInPointsProd x points =
        match points with
        | [] -> double 1
        | _::_ -> List.map (fun c -> x - c) points |> List.reduce mul
    let substituteInPoly x (poly : polynomial) = List.map (fun f -> f x) poly |> List.sum
    
    let getClosestPoints (xs : double list) x pointsCount =
        xs |> List.sortBy (fun v -> abs (v - x)) |> List.truncate pointsCount
    
    let private getWn1Derivative xs x =
        let substituted = xs |> List.map (fun c -> x - c)
        let n = List.length xs
        [0..n-1] |> List.map (fun i -> List.removeAt i substituted |> List.reduce mul) |> List.sum
        
    let private lagrangeApproximation (xs, ys) : polynomial = 
        let a_k k =
            let truncatedPoints = List.removeAt k xs
            let numerator x = substituteInPointsProd x truncatedPoints
            let denominator =  getWn1Derivative xs (List.item k xs)
            fun x -> (numerator x / denominator ) * (List.item k ys)
        let n = List.length xs
        [0..n-1] |> List.map a_k
        
    let kDividedDiffs (xs : double array) prevDiffs k =
        prevDiffs
        |> List.pairwise
        |> List.mapFold (fun i funcPair -> ((snd funcPair) - (fst funcPair)) / (xs[i+k] - xs[i]), (i+1)) 0
        |> fst
        
    let private newtonApproximation (xs, ys) : polynomial =
        let n = List.length xs
        let arrayXs = Array.ofList xs
        let mapFolder prevDiff k =
            let nextDiffs = kDividedDiffs arrayXs prevDiff k
            nextDiffs, nextDiffs
        let dividedDiffs = (List.head ys)::(List.mapFold mapFolder ys [1..n-1] |> fst |> List.map List.head)
        let substituteInPointsProd points x = substituteInPointsProd x points
        let diffsPairCoefficients = List.map (fun i -> List.take i xs |> substituteInPointsProd ) [0..n-1]
        List.map2 (fun divDiff c -> (fun x -> (c x) * divDiff)) dividedDiffs diffsPairCoefficients
        
    let getPolynomial form (xs,ys) =
        match form with
        | LagrangeForm -> lagrangeApproximation (xs,ys)
        | NewtonForm -> newtonApproximation (xs,ys)
        