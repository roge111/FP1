from sys import setrecursionlimit
setrecursionlimit(1_000_000)

def erla6Summ(summ: int, i: int) -> int:
    if i == 100:
        return summ
    else:
        return erla6Summ(summ + i, i + 1)
def erla6SummKv(summKv: int, i: int) -> int:
    if i == 100:
        return summKv
    else:
        return erla6SummKv(summKv + i**2, i + 1)
def result() -> int:
    summ = erla6Summ(0, 0)
    summKv = erla6SummKv(1, 1)
    if summ >= summKv:
        return summ - summKv
    else:
        return summKv - summ

print(result())