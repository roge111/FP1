from sys import setrecursionlimit
setrecursionlimit(10_000)

def fibonachi(arr: list, i: int):
    count = len(arr)
    if arr[count-1] > i:
        return arr[count-1]
    else:
        arr.append(arr[count-2] + arr[count-1])
        return fibonachi (arr, i)

result = fibonachi([1, 1], 1000)
print(result)