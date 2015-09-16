import nltk
import random
file = open('C:/Users/tianjiachuan/Desktop/1.txt','r')
data = file.read()
data = data.split()

def makePairs(data):
    result = [(data[i], data[i+1]) for i in range(len(data)-1)]
    return result

def generate(cfd, word = 'the', num = 100):
    result = ''
    for i in range(num):
        arr = []
        for j in cfd[word]:
            for k in range(cfd[word][j]):
                arr.append(j)
        result += ' '+ word
        word = arr[int((len(arr))*random.random())]
    return result        

cfd = nltk.ConditionalFreqDist(makePairs(data))
print generate(cfd) + '.'
