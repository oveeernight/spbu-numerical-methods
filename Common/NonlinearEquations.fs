namespace Common

open System

module NonlinearEquations =
    let findSegments (func : double -> double) a b h =
        let rec findSegmentsRec a b h prevSegmentsCount = 
            let h = h / double 10
            let list = [a..h..b]
            let suitable = List.pairwise list |> List.filter (fun s -> func (snd s) * func (fst s) < 0)
            let count = List.length suitable
            if count = prevSegmentsCount then suitable else findSegmentsRec a b h count
        findSegmentsRec a b h -1

    let private orderPair (a : double, b : double) =
        Math.Min(a,b), Math.Max(a,b)
        
    let private newtonMethodBase (func : double -> double) getNext eps x0 =
        let rec newtonMethodBaseRec prev stepsCount =
            let next = getNext prev
            if abs (func next - func prev) < 2.0 * eps then next, stepsCount, orderPair (prev, next) else newtonMethodBaseRec next (stepsCount + 1)
        newtonMethodBaseRec x0 0

    let newtonMethod func funcDer eps x0 segment =
        let getNext x = x - (func x) / (funcDer x)
        newtonMethodBase func getNext eps x0
    let newtonMethodAdvanced func funcDer eps x0 segment =
        let getNext x = x - (func x) / (funcDer x0)
        newtonMethodBase func getNext eps x0

    let secantMethod (func : double -> double) eps x0 x1 segment =
        let getNext prev prevPrev =
            prev - (prev - prevPrev) * (func prev) / ((func prev) - (func prevPrev)) 
        let rec secantMethodRec prev prevPrev stepsCount =
            let next = getNext prev prevPrev
            if abs (func next) < 2.0 * eps then next, stepsCount, orderPair (prev, next) else secantMethodRec next prev (stepsCount + 1)
        secantMethodRec x1 x0 0
        

    let bisectionMethod (func : double -> double) eps segment =
        let rec bisectionMethodRec eps stepsCount segment = 
            let m = double (snd segment + fst segment) / 2.0
            let leftValue = func (fst segment)
            let middleValue = func  m
            if abs (snd segment - fst segment) < 2.0 * eps then m, stepsCount, segment else
                if leftValue * middleValue < 0 then bisectionMethodRec eps (stepsCount + 1) (fst segment, m)
                else bisectionMethodRec eps (stepsCount + 1) (m, snd segment)
        bisectionMethodRec eps 0 segment