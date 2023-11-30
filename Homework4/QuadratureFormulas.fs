namespace Homework4

type polynomialBE = {coefficients: double list; degree: int}
    with
    member x.getSubstitutionValue (point : double) =
        List.mapFold (fun acc c -> acc * c, acc * point) (double 1) x.coefficients |> fst |> List.sum
    member x.definiteIntegral a b =
        let intPoly = {coefficients =  0::List.mapi (fun i c -> c / (double i + 1.0)) x.coefficients; degree = x.degree + 1}
        (intPoly.getSubstitutionValue b) - (intPoly.getSubstitutionValue a)
    static member poly0 = {coefficients = [6]; degree = 0}
    static member poly1 = {coefficients = [-1; 1]; degree = 1}
    static member poly2 = {coefficients = [5; -4; 1]; degree = 2}
    static member poly3 = {coefficients = [2; 3; -4; 2]; degree = 3}
    static member poly4 = {coefficients = [0; 3; 0; 0; 2]; degree = 4}

module QuadratureFormulas =
    let leftRectangle (func : double -> double) a b =
        (b - a) * func a

    let rightRectangle (func : double -> double) a b =
        (b - a) * func b

    let middleRectangle (func : double -> double) a b =
        (b - a) * func ((a + b) / 2.0)

    let trapezoid (func : double -> double) a b =
        (b - a) / 2.0 * (func a + func b)
    
    let simpson (func : double -> double) a b =
        (b - a) / 6.0 * (func a + 4.0 * func ((a + b) / 2.0) + func b)
        
    let frac38 (func : double -> double) a b =
        let h = (b - a) / 3.0
        (b - a) * ((func a) / 8.0 + func (a + h) * 3.0 / 8.0 + func (a + 2.0 * h) * 3.0 / 8.0 + (func b) / 8.0)
