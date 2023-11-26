namespace Homework3

open System

type pointSort =
    | Left
    | Middle
    | Right
    
type point = {value: double; sort: pointSort}

type interval = {points: point list}

module Interval =
    
    let private removeLast xs =
        let len = List.length xs
        List.removeAt (len-1) xs
    let fromList xs =
       let first = {value = List.head xs; sort = Left}
       let last = {value = List.last xs; sort = Right}
       let middle = List.skip 1 xs |> List.map (fun x -> {value = x; sort = Middle}) |> removeLast
       {points = List.concat [[first]; middle; [last]]}
       
    let calculateFirstDerivative (func : double -> double) h interval =
        let calculateOne point =
            match point.sort with
            | Left -> ((-3.0 * func point.value) + 4.0 * func (point.value + h) - func (point.value + 2.0 * h)) / (2.0 * h)
            | Right -> ((3.0 * func point.value) - 4.0 * func (point.value - h) + func (point.value - 2.0 * h)) / (2.0 * h)
            | Middle -> (func (point.value+h) - func (point.value-h)) / (2.0 * h)
            |> double
        List.map calculateOne interval.points
        
    let calculateSecondDerivative (func: double -> double) h interval =
        let calculateOne point =
            match point.sort with
            | Left
            | Right -> (0.0 / 0.0)
            | Middle -> ((func (point.value - h) - 2.0 * (func point.value)) + func (point.value + h)) / (h * h)
        List.map calculateOne interval.points
