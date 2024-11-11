module Tests

open Xunit

open projectElra25
open projectElra6

//Проверка результата вывода хвостовой рекурсивной функции
let ``fibonachiHvost searches for the first number greater than 1000`` () =

    let result = fibonachiHvost(1000)
    Assert.Equal(1597, result)

// Проверка результата вывода рекурсивной функцией
let ``loop_r searches for the first number greater than 1000`` () =
    let result  = loop_r([1; 1], 1000)
    Assert.Equal(1597, result)

//Проверка результата вывода рекурсивной ховстовой функции
let ``result finds the difference between the squared sum and the sum of the squares of numbers from 1 to 100``() =
    let res = result(100)
    Assert.Equal(323401, res)
//Проверка результата вывода рекурсивной функции

let ``result2 finds the difference between the squared sum and the sum of the squares of numbers from 1 to 100`` () =
    let res = result2(100)
    Assert.Equal(323401, res)
// Проверка результата вывода функции, реализованнно со специальным синтаксисом для цикла
let ``result3 finds the difference between the squared sum and the sum of the squares of numbers from 1 to 100``() =
    let res = result3(100)
    Assert.Equal(333300, res)


