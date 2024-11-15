

# **Лабортаорная работа 1**
**Выполнил**: Батаргин Егор Александрович

**Группа**: P3332

**Проблемы Эйлера**: 6 и 25
# Описание проблемы 6
  Проблема заключается в том, что нужно найти разницу между суммой квдратов и квадратом суммы числе от 1 до n
# Реализации проблемы 6
  Релаизация находиться в диреткории "projectElra6". Реалиазция выполнина двумя разными рекурсиями и с помощью специального синтаксиса. 
  
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

  **Специальный синтаксис**

  Так же была сделана реализация с помощью специального синтаксиса для циклов

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
    

  Так же приведу в пример аналогичную программу на традиционном языке программирования - Python

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

  Релаизация находиться в диреткории "projectElra25". Реализация выполняется так же с помощью хвостовой рекурсии и обычной рекурсии

    let fibonachiHvost (i: int): int = 
      let rec loop (acc: int list, i)  = 
              
              let mutable count = acc.Length
              if acc[count-1] >= i then acc[count-1]
              else
                  loop (List.append acc [acc[count-2] + acc[count - 1]], i)
    
      loop ([1; 1], i)

  Пометка rec означает в F# то, что функция рекурсивная.  Переменная _acc_ так же означает память аккумулятора

  А вот функция, реализованная обычной рекурсией
  
    let rec loop_r(array:int list, i: int): int =
      let mutable lastIndex = array.Length - 1
      if array[lastIndex] >= i then array[lastIndex]
      else
          loop_r(List.append array [array[lastIndex - 1] + array[lastIndex]], i)
  Переменная _i_ содержит число, больше которого мы ищем минимальное значение из чисел фибоначи. 

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
  
  


