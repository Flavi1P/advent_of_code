import numpy as np
input = "data/2024/input_2.txt"
dat = [list(map(int, l.split())) for l in open(input)]
def test_array(x):
    if np.all(np.diff(x) > 0) or np.all(np.diff(x) < 0):
        if np.all(abs(np.diff(x)) <= 3):
            return(True)
    else :
        return(False)
safe_reports = 0
for array in dat:
    if test_array(array):
        safe_reports += 1

print(f'First part answer is: {safe_reports}')
    
safe_reports = 0
for array in dat:
    if test_array(array):
        safe_reports += 1
    else:
        for i in range(len(array)):
            if test_array(np.delete(array, i)):
                safe_reports += 1
                break

print(f'Second part answer is: {safe_reports}')   