from gdc_and_lcm import *
def prop2():
    is_true = True
    for a in range(1,1000):
        for b in range(1,1000):
            if (12)%(2) and (a)%(2) == 0:    # left side
                is_true = (a)%(5) == 0  # right side
            if not is_true:
                break
        if not is_true:
            break
    print(is_true)

def prop3():
    is_true = True
    for a in range(1,100):
        for b in range(1,100):
            for c in range(1,100):
                if (b*c)%a == 0:    # left side
                    is_true = (lcm(b,c))%a == 0  # right side
                if not is_true:
                    break
            if not is_true:
                break
        if not is_true:
            break
    print(is_true)

def prop3andor():
    is_true = True
    for a in range(1,1000):
        for b in range(1,1000):
            for c in range(1,1000):
                if (b*c)%a == 0:                    # left side
                    is_true = b%a == 0 or c%a == 0  # right side
                if not is_true:
                    break
            if not is_true:
                break
        if not is_true:
            break
    print(is_true)

def prop3andorswiched():
    is_true = True
    for a in range(1,250):
        for b in range(1,250):
            for c in range(1,250):
                if b%a == 0 and c%b == 0:        # left side
                    is_true = (b-c)%(a) == 0  # right side
                if not is_true:
                    break
            if not is_true:
                break
        if not is_true:
            break
    print(is_true)

def singleprop2():
    is_true = True
    for a in range(1,1000):
        for b in range(1,1000):
            is_true = ((16*a)+(24*b))%(4) == 0  # right side
        if not is_true:
            break
    print(is_true)

def singleprop3():
    is_true = True
    for a in range(1,1000):
        for b in range(1,1000):
            for c in range(1,1000):
                is_true = (16)%(4) == 0  # right side
            if not is_true:
                break
        if not is_true:
            break
    print(is_true)
print("this doc")

print(gcd(12,18))

def find_b(a,m):
    i = -1000
    while i < 1000:
        if 4 == 59 + (11*i):
            print(i)
        i = i + 1
    print(i)

print(find_b(4,11))


print(4%11 == 48%11 )

print()

#prop2()
#prop3()
#prop3andor()
#prop3andorswiched()
#singleprop2()
#singleprop3()
    # a|b = (b)%(a)