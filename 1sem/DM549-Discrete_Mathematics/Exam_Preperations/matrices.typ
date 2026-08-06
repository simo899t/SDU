#import "@local/tempst:0.1.0": *
#show: note.with(
  title:         "Discrete Mathematics notes",
  subtitle:      "Matrices",
  course:        "DM549 - Discrete Mathematics",
  author:        "Simon Holm",
  date:          "Fall 2024",
  outline:       true,
  outline-depth: 2,
)

= Symmetry
#definition(title: "Definition: Matrix")[
  A matrix $A$ is a rectangular grid _(or array)_ of numbers, symbols, or expressions arranged in rows (horizontal lines) and columns (vertical lines).
  $ A^(m times n) = mat(a_(11), dots.c, a_(1n);
            dots.v, dots.down, dots.v;
            a_(m 1), dots.c, a_(m n)) $
]


#definition(title: "Definition: Symmetric matrix")[
  Let a matrix $A$ be _symmetric_, then
  $ a_(i,j) = a_(j,i) quad  forall i,j $
  This is equal to
  $ A = tran(A) $
]

== Operations on matrices
#definition(title: "Definition: Matrix Addition")[
  Let $A = mat(a_(i j))$ and $B = mat(b_(i j))$ both be $m times n$ matrices.

  The sum of $A$ and $B$, denoted as $A+B$ is another $m times n$ matrix that has $a_(i j) + b_(i j)$ as its $(i,j)$th entry. In other words this means that. $ A+B = mat(a_(i j) + b_(i j)) quad forall i,j $
]

#definition(title: "Definition: Matrix Multiplication")[
  Let $A$ be an $m times n$ matrix and $B$ be a $n times p$ matrix.

  Then $A B$

  
  

]
#pagebreak()

#example(title: "Example: Matrix Multiplication")[
  Given two matrices $A$ and $B$ we use the definition to compute $A B$

  We use the following rule
  #figure(
    image("assets/image-1.png", width: 20em),
    caption: [Matrix multiplication definition visualized],
  ) <label>
]

 