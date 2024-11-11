module projectElra25
// For more information see https://aka.ms/fsharp-console-apps


// Реализация модульной реализации с хвостовой рекурсией
let fibonachiHvost (i: int): int = 
    let rec loop (acc: int list, i)  = 
            
            let mutable count = acc.Length
            if acc[count-1] >= i then acc[count-1]
            else
                loop (List.append acc [acc[count-2] + acc[count - 1]], i)
    
    loop ([1; 1], i)



// Реализация рекурсивной программой
let rec loop_r(array:int list, i: int): int =
    let mutable lastIndex = array.Length - 1
    if array[lastIndex] >= i then array[lastIndex]
    else
        loop_r(List.append array [array[lastIndex - 1] + array[lastIndex]], i)


