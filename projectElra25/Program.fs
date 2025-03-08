module projectElra25

// Реализация модульной реализации с хвостовой рекурсией
let fibonachiHvost (i: int) : int =
    let rec loop (acc: int list, i) =
        match acc[acc.Length - 1] >= i with
        | true -> acc[acc.Length - 1]
        | false -> loop (List.append acc [ acc[acc.Length - 2] + acc[acc.Length - 1] ], i)

    loop ([ 1; 1 ], i)

// Реализация рекурсивной программой
let rec loop_r (array: int list, i: int) : int =
    match array[array.Length - 1] >= i with
    | true -> array[array.Length - 1]
    | false -> loop_r (List.append array [ array[array.Length - 2] + array[array.Length - 1] ], i)

// Реализация при помощи List.map (отображения)
let fibonachi (n: int) : int64 =
    let rec fibonachi_List (array: int64 list, i: int) : int64 list =
        match array.Length = i with
        | true -> array
        | false -> fibonachi_List (List.append array [ array[array.Length - 2] + array[array.Length - 1] ], i)

    let f (i) : int64 =
        match i with
        | 1
        | 2 -> 1
        | _ ->
            let arr: int64 list = fibonachi_List ([ 1; 1 ], i)
            arr[arr.Length - 1]

    let rec result (n: int, array: int64 list, i: int) : int64 =
        match array[i] >= n with
        | true -> array[i]
        | false -> result (n, array, i + 1)

    let numbers: int64 list = [ for i in 1..n -> i ]
    let fibolist = List.mapi (fun i x -> f (i + 1)) numbers
    let res: int64 = result (n, fibolist, 0)
    res

let finbonachiSeq (n: int) : int64 =
    let rec fibList (array: int64 list, x: int64) : int64 =
        match int64 (array.Length) = x with
        | true -> array[array.Length - 1]
        | false -> fibList (List.append array [ array[array.Length - 2] + array[array.Length - 1] ], x)

    let fib (x: int64) : int64 =
        match x with
        | 1L
        | 2L -> 1L
        | _ ->
            let res = fibList ([ 1; 1 ], x)
            res

    let numbers: int64 seq = Seq.initInfinite (fun x -> x)
    let fibolist = Seq.mapi (fun i x -> fib (x)) numbers

    let rec result (n: int, array: int64 seq, i: int) : int64 =
        match Seq.item i array >= n with
        | true -> Seq.item i array
        | false -> result (n, array, i + 1)

    let res: int64 = result (n, fibolist, 0)
    res
