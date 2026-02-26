import autograd as auto
import autograd.numpy as np
import matplotlib.pyplot as plt
import scipy.optimize as opt


def ex_rosenbrock(x,a):
    sum = 0

    n = x[0].shape()

    i = 1
    while i < n/2:
        first = x[2 * i] - (x[2 * (i - 1)])**2
        second = 1 - x[2 * (i - 1)]

        sum =+ a * first**2 + second**2
    return sum
    
nabla_f = auto.grad(ex_rosenbrock())


