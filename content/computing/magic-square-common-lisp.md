+++
title = "Solving the Magic Square in Common Lisp"
author = ["Aatmun Baxi"]
tags = ["lisp", "programming"]
draft = false
weight = 2002
type = "post"
+++

<div class="ox-hugo-toc toc">

<div class="heading">Table of Contents</div>

- [The Problem](#the-problem)
- [Helper Functions](#helper-functions)
    - [Summing List Values](#summing-list-values)
    - [Converting Between Grids and Flat Lists](#converting-between-grids-and-flat-lists)
    - [Grabbing Diagonals](#grabbing-diagonals)
    - [Checking Win Conditions](#checking-win-conditions)
    - [Range Generation](#range-generation)
    - [Pretty Printing](#pretty-printing)
- [The Solver](#the-solver)
- [Using the Solver](#using-the-solver)

</div>
<!--endtoc-->



## The Problem {#the-problem}

The magic square problem is a typical recursion exercise given to students learning programming.
Since I&rsquo;ve been playing around with common lisp recently, I thought I&rsquo;d use it as an excuse to learn a bit about the language.

A magic square (of size 3) is a 3x3 grid of numbers whose rows, columns, and diagonals sum up to the same number.
We&rsquo;ll be working with the simplest case here, where the grid can only be filled with values `1-9`, so the win condition is when the rows, columns, and diagonals sum up to `15`.
Our job is to write a program that takes as user input a grid of 9 numbers, possibly with some values filled out already, and return a solved magic square from those predetermined numbers.
If the square cannot be solved, we should return `nil`.


## Helper Functions {#helper-functions}

We&rsquo;ll model a 3x3 square as a flat list of 9 elements.
We index row-first.
For an example, the following table

```text
1 2 3
4 5 6
7 8 9
```

will be encoded as the list `(1 2 3 4 5 6 7 8 9)`.

In order to check if a square is a &ldquo;real&rdquo; magic square, we need some helper functions that will be used to check the win condition.


### Summing List Values {#summing-list-values}

The first thing we need is the ability to sum all values of a list.
This doesn&rsquo;t really require much to do, but it&rsquo;s convenient to have a succinct name to refer to.

```lisp
(defun sum (vals)
  "Computes sum of values of a list"
  (apply #'+ vals))
```


### Converting Between Grids and Flat Lists {#converting-between-grids-and-flat-lists}

The next things we need is the ability to &ldquo;flatten&rdquo; a list that encodes a grid.
Internally, we&rsquo;ll process everything as a 1 dimensional list, but we&rsquo;d like to display and let the user work with the lists in a nicer human readable format as a 3x3 grid.
For this we&rsquo;ll implement a function to convert the 2 dimensional grid into a 1 dimensional list.

```lisp
(defun flatten (list-of-lists)
  "Flatten a list of lists (single nested level)"
  (apply #'append list-of-lists))
```

We&rsquo;ve assumed strongly that the lists we use are length 9 and the grid we want is 3x3.
We might want to change this if we want a magic square solver for larger grids like 4x4 or 5x5.

We also implement a `transpose` function, whose use will become apparent soon.

```lisp
(defun transpose (square)
  "Transposes a square"
  (apply #'append (loop for i :from 0 :to 2 :collect
                        (apply #'append (loop for j :from 0 :to 2 :collect
                                              (list (nth (+ i (* j 3)) square)))))))
```

Note here that it should be trivial to extend these functions to work on arbitrarily-sized square by doing some logic on the bounds and hard-coded numbers within the function.
Not that you&rsquo;d want to, there&rsquo;s already 9! (9 factorial) permutations to check with just a 3x3 grid...


### Grabbing Diagonals {#grabbing-diagonals}

We also implement two functions that get the main diagonal and off-diagonal of a grid from a flat list.

```lisp
(defun main-diagonal (square)
  "Gets main diagonal of square"
  (list (nth 0 square) (nth 4 square) (nth 8 square)))

(defun off-diagonal (square)
  "Gets off diagonal of square"
  (list (nth 2 square) (nth 4 square) (nth 6 square)))
```


### Checking Win Conditions {#checking-win-conditions}

Now that we have functions to get the rows, columns, and diagonals, we can go about checking win conditions.
The win conditions in the 3x3 case are that the sum of all rows, columns, and diagonals equal `15`.

```lisp
(defun filled? (square)
  (not (member 0 square)))

(defun rows-solved? (square)
  "Checks if rows of square are solved"
  (let ((solved (loop :for i :from 0 :below 9 by 3 :collect
                ;; Magic number here only applies for 3x3 squares...
                   (eq 15 (sum (subseq square i (+ 3 i)))))))
    (every (lambda (x) (eq x t)) solved)))

(defun columns-solved? (square)
  "Checks if the columns of square are solved"
  (rows-solved? (transpose square)))

(defun diagonals-solved? (square)
  "Check if diagonals of square (flat list) are solved"
  (and (eq 15 (sum (main-diagonal square))) (eq 15 (sum (off-diagonal square)))))
```

We can chain these together to form a final `solved?` function, which checks if a square is a magic square.

```lisp
(defun solved? (square)
  "Determines if square is solved"
  (and (rows-solved? square) (columns-solved? square) (diagonals-solved? square)))
```


### Range Generation {#range-generation}

This neat little function emulates (to some extent) python&rsquo;s `range` function.
It makes use of the `loop` macro, which is wonderfully flexible.

```lisp
(defun range (max &key (min 0) (step 1))
 "Generates Python-like ranges"
  (loop for n from min below max by step
        collect n))
```


### Pretty Printing {#pretty-printing}

Just to aid visually, we&rsquo;ll write a function to print a flat list as a 3x3 grid.

```lisp
(defun print-square (square)
  (cond ((null square) (print nil))
        (t (format nil "~{~a ~a ~a~%~}" square))))
```


## The Solver {#the-solver}

The solver function should take in a grid-formatted 3x3 square with possibly-filled in values, with `nil` indicating unfilled values.
The solver should return a solved magic square, or `nil` otherwise.

The top-level `solve` function will format the grid into a flat list and pass it to an internal function `solve--internal` which will employ recursion to solve the grid.

```lisp
(defun solve (square)
  "Solves magic square from existing square"
  (let* ((flat-square (mapcar (lambda (x) (if (not x) 0 x) ) (flatten square)))
         (used (remove 0 (remove-duplicates flat-square)))
         (solutions nil))
    (solve--internal flat-square solutions 0 used)))
```

This function just maps `nil` to `0`, flattens the square, and initializes an internal solve, keeping track of the already-used values in the list.

The internal function looks like this

```lisp
(defun solve--internal (square solutions index used)
  (cond
    ;; entry is filled and we aren't at the end
    ((and (not (filled? square)) (not (eq 0 (nth index square))))
     (return-from solve--internal (solve--internal square solutions (+ index 1) used)))
    ;; at end with no solution
    ((and (filled? square) (not (solved? square)))
     (return-from solve--internal nil))
    ;; at end with solution
    ((solved? square) (return-from solve--internal square))
    ;; unfilled otherwise
    ((not (filled? square))
     (loop :for num :in (range 10 :min 1) :when (not (member num used))
           do
              (setf (nth index square) num)
              (push num used)
              (let* ((possible-sol (solve--internal square solutions (+ index 1) used)))
                (if (not (null possible-sol))
                    (return-from solve--internal possible-sol)
                    (progn (setf (nth index square) 0)
                           (setf used (remove num used))))))
     (return-from solve--internal solutions))))
```


## Using the Solver {#using-the-solver}

We can now solve some magic squares.

Let&rsquo;s solve a blank one first.

```lisp
(defvar solution (solve (list (list nil nil nil) (list nil nil nil) (list nil nil nil))))
(print-square solution)
```

```text
2 7 6
9 5 1
4 3 8
```

Note the function only returns the first square that it finds to be solved.

We can also solve squares with pre-filled values and check if a solution is possible.

```lisp
(print-square (solve (list (list nil nil 4) (list nil nil nil) (list nil nil nil))))
```

```text
2 9 4
7 5 3
6 1 8
```

Of course, there are also situations where a solution is not possible given pre-filled entries.

```lisp
(print-square (solve (list (list 9 nil 4) (list nil nil nil) (list nil nil nil))))
```

```text
NIL
```

Notice that any valid magic square also has its transpose as a valid magic square.
We can verify that our solver works by checking if we get a result by taking the transpose.

```lisp
(print-square (solve (list (list nil nil nil) (list nil nil nil) (list 4 nil nil))))
```

```text
2 7 6
9 5 1
4 3 8
```
