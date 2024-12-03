module projectElra25
// For more information see https://aka.ms/fsharp-console-apps


// Реализация модульной реализации с хвостовой рекурсией
let fibonachiHvost (i: int): int = 
    let rec loop (acc: int list, i)  = 
            
            
            if acc[acc.Length-1] >= i then acc[acc.Length-1]
            else
                loop (List.append acc [acc[acc.Length-2] + acc[acc.Length - 1]], i)
    
    loop ([1; 1], i)



// Реализация рекурсивной программой
let rec loop_r(array:int list, i: int): int =
    if array[array.Length - 1] >= i then array[array.Length - 1]
    else
        loop_r(List.append array [array[array.Length - 2] + array[array.Length - 1]], i)


//Реализация при помощи List.map (отображения)
let fibonachi (n: int): int64 = 
    //Генерация фибоначи до i-го элемента
    let rec fibonachi_List (array: int64 list, i: int): int64 list = 
        if array.Length = i then array
        else
            fibonachi_List(List.append array [array[array.Length - 2] + array[array.Length - 1]], i)
    //Вычисление нового элемента фибоначи
    let f(i): int64=
        let result:int  = 1
        if i = 1 || i = 2 then
            result
        else
            let arr :int64 list= fibonachi_List([1; 1], i)
            arr[arr.Length - 1]
    let rec result(n: int, array: int64 list, i: int): int64 = 
        if array[i] >= n then array[i]
        else
         result(n, array, i + 1)


    // Реализация с использование специального синтаксиса для циклов
    let numbers: int64 list = [for i in 1..n -> i ]

    let number:int64 list = [1; 1]
    let fibolist = List.mapi (fun i x -> f(i + 1)) numbers

    
            

    let res: int64 = result(n, fibolist, 0)
    res
//Реализация при помощи бесконечного списка
let finbonachiSeq(n: int): int64=

    
    let rec fibList(array:int64 list, x: int64): int64 =
        if int64(array.Length) = x then array[array.Length - 1]
        else
            fibList(List.append array [array[array.Length - 2] + array[array.Length - 1]], x)

    let fib(x: int64): int64 =
        if x = 1 || x = 2 then 1
        else 
            let res = fibList([1; 1], x)
            res
    
    let numbers: int64 seq = Seq.initInfinite(fun x -> x)

    
    let fibolist = Seq.mapi (fun i x -> fib(x)) numbers

    let rec result(n: int, array: int64 seq, i: int): int64 = 
        if Seq.item i array >= n then Seq.item i array
        else
         result(n, array, i + 1)
            

    let res: int64 = result(n, fibolist, 0)
    res
