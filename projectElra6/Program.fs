module projectElra6
// Реализация модульной реализации с хвостовой рекурсией


let result(n: int): int =

    let elra6Summ(n: int): int  = 
        let rec accElra6Summ(acc: int, i: int, n: int): int =
            if i = n then acc + 1
            else
                accElra6Summ (acc + i, i + 1, n)
        accElra6Summ(0, 0, n)

    let elra6SummKv(n: int): int = 
        let rec accElra6SummKv(acc: int, i: int, n: int): int = 
            if i = n then acc + 
            else
                accElra6SummKv(acc + i * i, i + 1, n)
        accElra6SummKv(1, 1, n)

    let summ: int = elra6Summ(n)
    let summKv: int = elra6SummKv(n)
    if summ >= summKv then 
        summ - summKv
    else
        summKv - summ 
    



// Реализация рекурсивной программой


let result2(n: int): int = 
    let rec Elra6SummKv(summ: int, i: int, n: int): int = 
            if i = 100 then summ + 1
            else
                Elra6SummKv(summ + i * i, i + 1, n)


    let rec Elra6Summ(summ: int, i: int, n: int): int =
        if i = 100 then summ + 1
        else
            Elra6Summ (summ + i, i + 1, n)
    let summ: int  = Elra6Summ(0, 0, n)
    let summKv  = Elra6SummKv(1, 1, n)
    if summ >= summKv then
        summ - summKv
    else
        summKv - summ


//Реализация при помощи List.map и специального синтакиса для циклов
let result3(n: int): int =
    let summList = [for i in 1..n -> i]
    let summKvList = List.map(fun x -> x * x) summList

    let summ = List.sum summList
    let summKv = List.sum summKvList

    if summ >= summKv then
        summ - summKv
    else
        summKv - summ
