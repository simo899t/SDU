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

= Operations on matrices
#definition(title: "Definition: Matrix Addition")[
  Let $A = mat(a_(i j))$ and $B = mat(b_(i j))$ both be $m times n$ matrices.

  The sum of $A$ and $B$, denoted as $A+B$ is another $m times n$ matrix that has $a_(i j) + b_(i j)$ as its $(i,j)$th entry. In other words this means that. $ A+B = mat(a_(i j) + b_(i j)) quad forall i,j $
]

#definition(title: "Definition: Matrix Multiplication")[
  Let $A$ be an $m times n$ matrix and $B$ be a $n times p$ matrix.

  Then $A B$ is performed as following
  #figure(
    image("assets/image-1.png", width: 20em),
    caption: [Matrix multiplication definition visualized],
  ) <label>
  
  

]


#example(title: "Example: Matrix Multiplication")[
  Given two matrices $A$ and $B$

  $ A = mat(1, 2, 3; 4, 5, 6; 7, 8, 9), quad B = mat(9, 8, 7; 6, 5, 4; 3, 2, 1) $

  Use the definition to compute $A B$

  $ A B = mat(1, 2, 3; 4, 5, 6; 7, 8, 9) dot mat(9, 8, 7; 6, 5, 4; 3, 2, 1) = mat(30, 24, 18; 84, 69, 54; 138, 114, 90) $

]

#definition(title: "Definition: Matrix Transpose")[
  The transpose of a matrix is obtained by interchanging its rows and columns
  $ tran(A) def "rows" <-> "columns" $

  Given that $A = tran(A)$, $A$ is called _symmetric_.
]

#example(title: "Example: Matrix Transpose")[
    Given Matrix 
    $ A = mat(1,2,3;4,5,6) -> tran(A) = mat(1,4;2,5;3,6) $
  ]

#definition(title: "Definition: Binary AND (meet) on matrices")[
  The $and$ (AND) operation on binary matrices are defined as individual $and$ operations on each of the two matrices entries

  This is also called meet, as in $A$ meets $B$

  $ A and B = mat(a_11,a_12,a_13;
        a_21,a_22,a_23;
        a_31,a_32,a_33) and 
        mat(b_11,b_12,b_13;
            b_21,b_22,b_23;
            b_31,b_32,b_33) = 
        mat(a_11 and b_11,a_12 and b_12,a_13 and b_13;
            a_21 and b_21,a_22 and b_22,a_23 and b_23;
            a_31 and b_31,a_32 and b_32,a_33 and b_33) $  
]

#definition(title: "Definition: Binary OR (join) on matrices")[
  The $or$ (OR) operation on binary matrices are defined as individual $or$ operations on each of the two matrices entries

  This is also called join, as in $A$ joins $B$

  $ A or B = mat(a_11,a_12,a_13;
        a_21,a_22,a_23;
        a_31,a_32,a_33) or 
        mat(b_11,b_12,b_13;
            b_21,b_22,b_23;
            b_31,b_32,b_33) = 
        mat(a_11 or b_11,a_12 or b_12,a_13 or b_13;
            a_21 or b_21,a_22 or b_22,a_23 or b_23;
            a_31 or b_31,a_32 or b_32,a_33 or b_33) $


]

#definition(title: "Definition: Boolean product on matrices")[
  $A dot.o B$ or $A circ B$ is calculated like standard matrix multiplication, but ordinary multiplication and addition are replaced by logical operations: logical AND ($and$) for multiplication and logical OR ($or$) for addition.
  $ A dot.o B = mat(a_11,a_12,a_13;
        a_21,a_22,a_23;
        a_31,a_32,a_33) dot.o
        mat(b_11,b_12,b_13;
            b_21,b_22,b_23;
            b_31,b_32,b_33) $

  #text(size: 8pt)[
    $ = mat(
              (a_11 and b_11) or (a_12 and b_21) or (a_13 and b_31),
              (a_11 and b_12) or (a_12 and b_22) or (a_13 and b_32),
              (a_11 and b_13) or (a_12 and b_23) or (a_13 and b_33);
              (a_21 and b_11) or (a_22 and b_21) or (a_23 and b_31),
              (a_21 and b_12) or (a_22 and b_22) or (a_23 and b_32),
              (a_21 and b_13) or (a_22 and b_23) or (a_23 and b_33);
              (a_31 and b_11) or (a_32 and b_21) or (a_33 and b_31),
              (a_31 and b_12) or (a_32 and b_22) or (a_33 and b_32),
              (a_31 and b_13) or (a_32 and b_23) or (a_33 and b_33)) $
  ]
]

#pagebreak()

= Concepts
#definition(title: "Definition: Matrix")[
  A matrix $A$ is a rectangular grid _(or array)_ of numbers, symbols, or expressions arranged in rows (horizontal lines) and columns (vertical lines).
  $ A^(m times n) = mat(a_(11), dots.c, a_(1n);
            dots.v, dots.down, dots.v;
            a_(m 1), dots.c, a_(m n)) $
]




#definition(title: "Definition: Symmetric matrices")[
  Let a matrix $A$ be _symmetric_, then
  $ a_(i,j) = a_(j,i) quad  forall i,j $
  This is equal to
  $ A = tran(A) $
]

#definition(title: "Definition: Binary matrices")[
  Let all entries of matrix $A$ only take binary values $0$ and $1$
  
  One can think of each entry as a logical value where $1= "TRUE"$ and  $0="FALSE"$
]

#definition(title: "Definition: Matrix Inverse")[
  Much like a function, some matrices have an inverse matrix.

  A square matrix $A^(n times n)$ is called _invertible_ (or _non-singular_) if there exists a matrix $A^(-1)$ such that
  $ A inv(A) = inv(A) A = I_n $
  where $I_n$ is the $n times n$ identity matrix.

  Given a simple  matrix $A in RR^(2times 2)$ where $A = mat(a,b;c,d)$ one can determine $ inv(A) = 1/(a c-b d) dot mat(d,-b;-c,a) $

  If no such $A^(-1)$ exists, $A$ is called _singular_ (or _non-invertible_).
]
