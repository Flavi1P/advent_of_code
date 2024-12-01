import numpy as np

dat = np.loadtxt('data/2024/input_1.txt')

list1 = np.sort(dat[:,0].astype(int))
list2 = np.sort(dat[:,1].astype(int))
res = sum(abs(list1 - list2))
print(res)

number, multiplier = np.unique(list2, return_counts=True)
lookup = dict(zip(number, multiplier))

def multiply(x, rule):
    out = x * rule.get(x, 0)
    return(out)
        

list3 = np.array([multiply(xi, lookup) for xi in list1])
print(sum(list3))
