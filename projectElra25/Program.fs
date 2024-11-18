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



