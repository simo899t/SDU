
def makeList(n) -> list:
    result = []
    for i in range(n,-1,-1):
        result.append(i)
    return result

def increaseKey(list, key, n, d) -> list:
    '''increases a key in a maxheap by n.'''
    arr = list
    arr[key] = arr[key] + n
    while key > 0:
        parent = (key - 1) // d
        if arr[key] > arr[parent]:
            arr[key], arr[parent] = arr[parent],  arr[key]
            key = parent
        else:
            break
    return arr

print(increaseKey(makeList(10), 8, 10, 3))