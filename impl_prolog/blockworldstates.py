import operator as op
from functools import reduce
from math import factorial


def ncr(n, r):
    """
    This function returns n Chooses r, i.e. C^{n}_{r}
    """
    r = min(r, n-r)
    numer = reduce(op.mul, range(n, n-r, -1), 1)
    denom = reduce(op.mul, range(1, r+1), 1)
    return numer / denom

def g(n, k):
    """
    This function returns g(n, k), i.e. number of states with k ground blocks
    and n unground blocks
    This is only defined for k > 0
    """
    res = 0
    for i in range(n+1):
        numerator = factorial(n+k-1)
        denominator = factorial(i+k-1)
        res += ncr(n, i) * numerator / denominator     
    return res


def L(k, n, x):
    """
    This function returns L^{k}_{n}(x)
    """
    res = 0
    for i in range(n+1):
        res += ncr(n,i)*factorial(n+k)/factorial(n)/factorial(i+k)*(-x)**i
    return res

def g2(n, k):
    """
    This function returns g(n, k), i.e. number of states with k ground blocks
    and n unground blocks
    This is only defined for k > 0
    """
    return factorial(n)*L(k-1, n, -1)

def g3(n, k):
    """
    THIS WORKS!!!!! FOR K=0!!!!
    """
    if n == 0:
        return 1
    else:
        return g3(n-1, k+1)+(n-1+k)*g3(n-1,k)


print("10 blocks ," + str(g3(10,0)) + " states. ")
for i in range(1, 11):
    print(str(i) + " blocks : " + str(g3(i,0)) + " states.")
