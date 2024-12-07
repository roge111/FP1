

# **Лабортаорная работа 1**
**Выполнил**: Батаргин Егор Александрович

**Группа**: P3332

**Проблемы Эйлера**: 6 и 25
# Описание проблемы 6
  Проблема заключается в том, что нужно найти разницу между суммой квдратов и квадратом суммы числе от 1 до n
# Реализации проблемы 6
  Релаизация находиться в диреткории "projectElra6". Реалиазция выполнина двумя разными рекурсиями и с помощью специального синтаксиса и генерации последовательности при помощи map. 
  
  **Хвостовая рекурсия**

  Осбеность хвостовой рекурсии заключается в том, что последней операцией является вызов рекурсивной функции. Но это еще не все

  Приведу в пример хвостовую рекурсию, находящую сумму числе от 1 до 100:

    let elra6Summ(n: int): int  = 
      let rec accElra6Summ(acc: int, i: int, n: int): int =
          if i > n then acc
          else
              accElra6Summ (acc + i, i + 1, n)
      accElra6Summ(0, 0, n)
  Здесь задействуется память аккумулятора, что позволяется расширить память для рекурсии. За память аккумулятора отвечает acc
  
  А вызов рекрусивной функции является последней операции функции

  **Рекурсивная функция**

  А обычная рекурсивная функция работает по принципу того, что она вызвает сама себя, пока не встретит условие, которое возвращает значение(-я)

  Ниже у меня приведена реализации рекурсивной функции 

    let rec Elra6SummKv(summ: int, i: int, n: int): int = 
            if i > n then summ
            else
                Elra6SummKv(summ + i * i, i + 1, n)
  Выше реализации нахождении суммы квадратов чисел от 1 до n.

  Дальше пример реализации с использованием специального синтаксиса и генерации последовательности при использовании map
    
    let result3(n: int): int =
      let summList = [for i in 1..n -> i]
      let summKvList = List.map(fun x -> x * x) summList
  
      let summ = List.sum summList
      let summKv = List.sum summKvList
  
      if summ >= summKv then
          summ - summKv
      else
          summKv - summ
  Здесь  происходит генерация чисел "summList", где будут числа от 1 до n. Дальше используя этот список происходит генерация нового списка. С помощью map мы забираем
  элемент из _summList_, а потом возводим в квадрат и добавляем в новый список _summKvList_. Далее с помощью _List.sum_  суммируем числа двух списком и вычисляем результат. 

  Далее идет пример реализации при помощи генерации бесконечного списка

    let result4(n: int): int =

      let rec summ(n: int, i: int, s: int, data: int seq): int =
          if i = n - 1 then s+ (Seq.item i data)
          else summ(n, i + 1, s+ (Seq.item i data), data)
      
  
      let data = Seq.initInfinite(fun index -> index)
      let data_sq = Seq.initInfinite(fun x -> x * x)
  
      let sum = summ(n, 0, 0, data)
      let sumSq = summ(n, 0, 0, data_sq)
  
      if sum >= sumSq then
          sum - sumSq
      else
          sumSq - sum
  Здесь два раза происходит генерация при помощи _Seq.initInfinite_. В первой генерации мы просто генерируем обычные числа от 1, а во второй - квараты чисел от 1. Функция summ реализует вычисление суммы. Функция сделана для того, чтобы вычислить сумму ограниченого количества чисел. Применяем функцию для обоих списков, а затем вичсляем результат.

  Так же приведу в пример аналогичную программу, реализованную с помощью рекурсии, на традиционном языке программирования - Python

      def erla6Summ(summ: int, i: int) -> int:
        if i == 100:
            return summ
        else:
            return erla6Summ(summ + i, i + 1)
  
  
  # Описание проблемы 25

  Проблема 25 заключается в том, что надо найти числа фибоначи. И узнать, индекс первого числа, которое больше n-го числа.

  Числа фибоначи - числа, которые равны сумме двух предыдущих. Например

    [1, 1, 2, 3, 5, 8, 13, 21]

   # Реализация проблемы 25

  Релаизация находиться в диреткории "projectElra25". Реализация выполняется так же с помощью хвостовой рекурсии и обычной рекурсии. Так же имеется реализация при специального синтаксиса и генерацией с использованием map, а так же с использованием генерации бесконечного списка

    // Реализация модульной реализации с хвостовой рекурсией
    let fibonachiHvost (i: int): int = 
        let rec loop (acc: int list, i)  = 
            if acc[acc.Length-1] >= i then acc[acc.Length-1]
            else
                loop (List.append acc [acc[acc.Length-2] + acc[acc.Length - 1]], i)
    
    loop ([1; 1], i)


  Пометка rec означает в F# то, что функция рекурсивная.  Переменная _acc_ так же означает память аккумулятора

  А вот функция, реализованная обычной рекурсией
  
    let rec loop_r(array:int list, i: int): int =
      if array[array.Length - 1] >= i then array[array.Length - 1]
      else
          loop_r(List.append array [array[array.Length - 2] + array[array.Length - 1]], i)
  Переменная _i_ содержит число, больше которого мы ищем минимальное значение из чисел фибоначи. 

  Далее пример реализации с генерацией при помощи _map_  и со спец. синтакисом. 

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
  
      
      let fibolist = List.mapi (fun i x -> f(i + 1)) numbers
  
      
              
  
      let res: int64 = result(n, fibolist, 0)
      res
  _int64_ - используется для вычисления больших чисел. _numbers_ создается с помощью спец. синтаксиса для циклов. Дальше создается список _fiblist_ с генерацией _List.mapi_ (то же самое, что и map, но помимо самого числа x есть индекс i). Функция _f_ и _fibonachi_List_ помогают вычислить новые элемент списка фибоначи.

  Далее пример реализации с генерацией бесконечного списка

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
              
  
      
  Данная функция будет работать долго, так как функция _fibList_ каждый раз с нуля будет реализовывать новый список фибоначи. Сама реализация - пример использования генерации бесконечного списка. Однако она работает долго
  
  Ниже пример реазации обычной рекурсивной функции для этой проблемы на языке Python

    def fibonachi(arr: list, i: int):
      count = len(arr)
      if arr[count-1] > i:
          return arr[count-1]
      else:
          arr.append(arr[count-2] + arr[count-1])
          return fibonachi (arr, i)
  
      result = fibonachi([1, 1], 1000)
# Вывод

В ходе лабораторной работы 1 я изучил базовый синтаксис функционального языка программирования F#. Немного было непривычно добавлять элементы в массив. В ходе изучения я понял, 

что при добалвении в список нового элемента в массив создается новый список, что занимает память. В целом удобный и понятный синтакис. 
  
  


