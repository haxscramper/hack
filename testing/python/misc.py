from IPython.display import display, SVG, Image
from fractions import Fraction
import pandas as pd
import numpy as np
import sympy as sp
import lcapy as lc
from math import *
import cmath as cm
from mpl_toolkits.mplot3d import Axes3D

import itertools

# TODO determine rounding for each value separately to produce best results
def round_expr_floats(expression, round_digits = 3):
    """Round all float literals to n decimal digits `1.21234x -> 1.212x`"""
    ex2 = expression
    for a in sp.preorder_traversal(expression):
        if isinstance(a, sp.Float):
            ex2 = ex2.subs(a, round(a, round_digits))
            
    return ex2

def print_sols_rounded(solution, round_digits = 3):
    for variable in solution:
        sol = round_expr_floats(solution[variable], round_digits)
        if isinstance(sol, sp.Float):
            print(f"{variable}: {sol:.{round_digits}f}")
        else:
            print(f"{variable}:", sol)
            
def subst_netlist_vals(netlist_string, val_dict):
    """Substitute values of components for netlist from value dictionary"""
    netlist_w_vals = ""
    str_vals = {}
    for k,v in val_dict.items():
        str_vals[str(k) + ""] = v

    for line in netlist_string.split("\n"):
        words = line.split(" ")   
        result = []
        for word in words:
            if word.endswith(";") and words[0] in str_vals:
                word = word[0:-1] + " " + str(str_vals[words[0]]) + ";"

            result.append(word)

        netlist_w_vals += " ".join(result) + "\n"
        
    return netlist_w_vals

def showcircuit(circuit, name):
    """convert circuit image to png and show in full size"""
    circuit.draw("/tmp/" + name + ".png", style="american")
    display(Image("/tmp/" + name + ".png"))
    
def memoize(f):
    """Decorator for output memoization"""
    memo = {}
    def helper(x):
        if x not in memo:            
            memo[x] = f(x)
        return memo[x]
    return helper


def sym(arg):
    return sp.symbols(arg)

def subst_dataframe(dataframe, expression, series_name, as_series = True):
    """subsitute values of each row of dataframe into symbolic expression"""
    
    symbolic = {sym(name): name for name in dataframe.columns}
    values = []
    for name, row in dataframe.iterrows():
        val_dict = {
            var_sym: row[var_name] for var_sym, var_name in symbolic.items()
        }
        
        values.append(expression.subs(val_dict))
        
    if as_series:
        return pd.Series(values, name=series_name)
    else:
        return values

def chain_subst_df(expression, series_name, dataframes, 
                   as_series = True):
    
    expressions = [expression]
    
    for frame in dataframes:
        substituted = []
        for expr in expressions:
            substituted.append(subst_dataframe(frame, expr, series_name, as_series=False))
        
        expressions = list(itertools.chain(*substituted))

    if as_series:
        return pd.Series(expressions, name=series_name)
    else:
        return expressions
    
    
def smart_round(num, digits = 2):
    if abs(num) == 0:
        return 0
    
    if abs(num) <= 1:
        left_shift = 0
        while abs(num) < (10 ** digits) - 1:
            num *= 10
            left_shift += 1
        
        num /= 10
        return round(num, 0) * pow(10, -left_shift + 1)
    else:
        right_shift = 0
        while abs(num) > 1:
            num /= 10
            right_shift += 1
            
        return round(num, 2) * pow(10, right_shift)
    

def clean(num):
    if isinstance(num, complex):
        if (num.real != 0) and (abs(num.imag / num.real) < (1 / (10 ** 6))):
            return smart_round(num.real)
        else:
            return complex(smart_round(num.real), smart_round(num.imag))
    else:
        return smart_round(num)
    
def e_d(deg):
    return cm.exp(complex(0, radians(deg)))

def magnitude(num):
    return sqrt(num.real ** 2 + num.imag ** 2)

def showval(val, l = None):
    # TODO print pandas dataframe
    if isinstance(val, complex):
        mod = magnitude(val)
        arg = cm.phase(val)
        
        if l:
            l = f"{l:<5}: "
        else:
            l = ""
        
        print(f"{l}{clean(val):^12} |z| = {clean(mod):<6} arg(z) = {clean(arg)}")
    else:
        print(val)
    
def tryclean(val):
    try:
        res = msc.clean(val)
        return res
    except:
        return val

    
def test_clean(num):
    print(clean(num))
    
    

def contour_2d(x_range, y_range, func, 
               cmap = 'Spectral', 
               figsize = (16, 12),
               ctype = 'filled', 
               rotate_axis = False
              ):
    w1_val = None
    if rotate_axis:
        w1_val = np.array([[func(x, y) for y in y_range] for x in x_range])
    else:
        w1_val = np.array([[func(x, y) for x in x_range] for y in y_range])
        
    
    if figsize:
        plt.figure(figsize=figsize)
        

    if rotate_axis:
        x_range, y_range = y_range, x_range
        
    if ctype == 'filled':
        plt.contourf(x_range, y_range, w1_val, 20, cmap = cmap)
    elif ctype == 'line':
        plt.contour(x_range, y_range, w1_val, 20, cmap = cmap)
        
        
def surface_3d(x_range, y_range, func, 
               figsize = (16, 12)):
    
    
    fig = plt.figure(figsize=figsize)
    
    ax = fig.add_subplot(111, projection='3d')
    X, Y = np.meshgrid(x_range, y_range)
    fun = np.vectorize(func)
    zs = np.array(fun(np.ravel(X), np.ravel(Y)))
    Z = zs.reshape(X.shape)

    ax.plot_surface(X, Y, Z, cmap='viridis')


def lsp(start, end, subdiv = 5):
    return np.linspace(start, end, int(abs(end - start) * subdiv))

if __name__ == '__main__':
    test_clean(10)
    test_clean(100)
    test_clean(100001)
    test_clean(0.21)
    test_clean(1.12 + 1.3j)
    test_clean(1.12)
    test_clean(1.3j)
    test_clean(0.307)
    test_clean(0.0000022222)

# test
