from scipy import optimize
from math import e, cos, sin
from matplotlib import pyplot as plt

def f(args):
    x = args[0]
    y = args[1]
    return ((e ** x)*(cos(x) + y*sin(x)))


res = optimize.minimize(
    fun=lambda x: -f(x), x0=[0, 0], bounds=((0, 10), (0, 10)))

print(res.x, f(res.x))
