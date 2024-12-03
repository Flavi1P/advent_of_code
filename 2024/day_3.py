import re
import numpy as np

with open('data/2024/input_3.txt', 'r') as file:
    dat = file.read().replace('\n', '')

    
def multiply_mult(x):
    nums = np.array(re.findall('\\d+', x)).astype(int)
    multiplied = np.multiply(nums[0], nums[1])
    return(multiplied)

def add_multiplications(long_string):
    lookup = re.findall('mul\\(\\d+,\\d+\\)', long_string)
    multiplications = list()
    for string in lookup:
        multiplied = multiply_mult(string)
        multiplications.append(multiplied)
    return(sum(multiplications))

#Part 1
res = add_multiplications(dat)
print(f'First answer {res}')
#Part 2
new_dat = re.sub('don\'t\\(\\).*?do\\(\\)', 'do()', dat)

res2 = add_multiplications(new_dat)
print(f'Second answer {res2}')