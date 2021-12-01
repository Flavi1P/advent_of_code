import numpy as np      

def readFile(fileName):
        fileObj = open(fileName, "r") #opens the file in read mode
        words = fileObj.read().splitlines() #puts the file into an array
        fileObj.close()
        return words

input = readFile("input.txt")

input = np.array(input, dtype = int)


def solution(start):
        tot = len(start)
        count = 1
        nocount = 0
        for i in range(0, tot):
                if start[i] > start[i - 1]:
                        count = count + 1
                else:
                        nocount = nocount +1
        return(count)

tot_new = len(input) - 3
input_bis = np.empty([tot_new, 1], dtype = int)

for i in range(0, tot_new):
        t_list = input[i:i+3]
        input_bis[i] = sum(t_list)


sol1 = solution(input)
sol2 = solution(input_bis)

print(sol1)
print(sol2)