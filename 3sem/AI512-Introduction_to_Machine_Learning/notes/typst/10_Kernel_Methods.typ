#import "@local/tempst:0.1.0": *
#import "@preview/mitex:0.2.7": mi, mimath

#show: note.with(
  title: "Kernel Methods",
  course: "AI512 — Introduction to Machine Learning",
  author: "Simon Holm",
  date: "2026-09-04",
)

= Transforming the input space

Suppose that we have a dataset where we can't easily classify data using a support vector (4. Support Vector Machines (SVM)). We can transform the space using a transformation function, to model the data by using more dimentions. Take the example from the lectures

$T(x,y)=(x^2,y^2,sqrt(2)x^2 y^2)$

// [image omitted: Pasted image 20260122111142.png]

This way we can use a hyperplane of $+1$ dimensionality to classify the data.


= Kernel Trick

Suppose that $T(x)$ is a mapping to a higher dimension

Note that algorithms like SVM only need *dot products*

That means that with
#mimath(`k(x_i,x_j) = \phi(x_i)^T\phi(x_j)`)

Because of the definition of the inner product, we can describe the relation between two points using $k(x_i,x_j)$


Computing #mi(`\phi(x)`) explicitly can be *very expensive or infinite-dimensional*

- *Linear Kernel:* #mi(`k(x,y) = x^\top y`)
- *Polynomial Kernel:* #mi(`k(x,y) = \left( x^\top y + c \right)^d`)
- *Radial Basis Function (RBF) Kernel:* #mi(`k(x,y) = \exp(-\frac{\| x-y \|^2}{2\sigma^2})`)
- *Sigmoid Kernel:* #mi(`k(x,y) = \tanh\left( \gamma x^\top y + c \right)`)


= Kernel regression

_(no notes yet)_


= Support Vector Machines (SVM)

Among many possible hyperplanes that may separate the data points of two classes, SVM chooses the one that maximizes the smallest distance between a data point and the hyperplane. 

This distance is called the *margin*. The data points that are closest to the hyperplane are called *support vectors*.

Note that we use because of these property, SVM is also called a *maximum margin classifier*. We would normally use k-fold cross validation, and balance the bias-variance tradeoff to ensure a maximum margin.

// [image omitted: Pasted image 20260120175429.png]
In this example is 2d, where the support vectors are lines

