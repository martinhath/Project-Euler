memo = {}


def penger(lst, N):
    if N == 0:
        return 1
    if len(lst) == 0 or lst[0] > N:
        return 0
    try:
        return memo[(lst[0], N)]
    except:
        ret = 0
        n = N // lst[0]
        for i in range(n + 1):
            ret += penger(lst[1:], N - (lst[0] * i))
        memo[(lst[0], N)] = ret
        return ret

mynter = [200, 100, 50, 20, 10, 5, 2, 1]
print(penger(mynter, 200))
