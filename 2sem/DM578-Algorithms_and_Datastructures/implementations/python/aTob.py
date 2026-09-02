def aTob(n, k, w, a, b) -> list:
    l = [0]*n
    for i in range(0,n):
        l[w[i]] = l[w[i]] + 1
    for i in range(1,n):
        l[i] = l[i] + l[i-1]
    return l[b-1]-l[a]
print(aTob(5,7,[0,0,2,3,4],0,4))