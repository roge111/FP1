module projectElra6
// Реализация модульной реализации с хвостовой рекурсией


let result(n: int): int =

    let elra6Summ(n: int): int  = 
        let rec accElra6Summ(acc: int, i: int, n: int): int =
            if i = n then acc
            else
                accElra6Summ (acc + i, i + 1, n)
        accElra6Summ(0, 0, n)

    let elra6SummKv(n: int): int = 
        let rec accElra6SummKv(acc: int, i: int, n: int): int = 
            if i = n then acc
            else
                accElra6SummKv(acc + i * i, i + 1, n)
        accElra6SummKv(1, 1, n)

    let summ: int = elra6Summ(n)
    let summKv: int = elra6SummKv(n)
    let mutable res =0
    if summ >= summKv then 
        res <-  summ - summKv
    else
        res <- summKv - summ 
    res
// Реализация с использованием специального синтаксиса для цикла
let result3(n: int): int =
    let mutable summ: int = 0
    let mutable summKv: int = 0
    for i in 1 .. n do
        summ <- summ + i
        summKv <- summKv + i * i
    
    
    if summ > summKv then
        summ - summKv
    else
        summKv - summ


// Реализация рекурсивной программой


let result2(n: int): int = 
    let rec Elra6SummKv(summ: int, i: int, n: int): int = 
            if i = 100 then summ
            else
                Elra6SummKv(summ + i * i, i + 1, n)


    let rec Elra6Summ(summ: int, i: int, n: int): int =
        if i = 100 then summ
        else
            Elra6Summ (summ + i, i + 1, n)
    let summ: int  = Elra6Summ(0, 0, n)
    let summKv  = Elra6SummKv(1, 1, n)
    let mutable res: int  = 0
    if summ >= summKv then
        res <- summ - summKv
    else
        res <- summKv - summ
    res
