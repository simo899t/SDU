def makeList(n) -> list:
    result = []
    for i in range(n,-1,-1):
        result.append(i)
    return result

def insert(list, key, n, d) -> list:
    '''insert a key in a maxheap.'''
    arr = list
    arr[key] = n
    while key > 0:
        parent = (key - 1) // d
        if arr[key] > arr[parent]:
            arr[key], arr[parent] = arr[parent],  arr[key]
            key = parent
        else:
            break
    return arr

print(insert(makeList(10), 8, 10, 3))