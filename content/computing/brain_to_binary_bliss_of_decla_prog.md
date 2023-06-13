+++
title = "Declarative Programming for UltraFast™ Brain-to-Binary"
author = ["Aatmun Baxi"]
draft = false
weight = 2004
type = "post"
+++

Recently I&rsquo;ve been messing around with different programming languages and branching out beyond the C-style languages I know like C++ and Python.
In my search for one to try out, I discovered Haskell, and the paradigm of functional programming as a whole.

As a mathematician by trade, the usefulness of programming was never lost on me.
From my time with C++ and Python, I had developed a (not unreasonable) notion that programming was all about telling computers how to do something you want done..
This changed when I learned of the functional programming paradigm, or more generally, declarative-style languages.

Suppose we wanted to write a function `fun` that takes in a non-negative integer and returns the sum of squares of numbers between 0 and that integer.
In fancy math language, the function would look like this:

\begin{align\*}
\mathtt{fun} : \mathbf{Z}\_{ \geqslant 0} &\to \mathbf{Z}\_{\geqslant 0} \\\\
                                \mathtt{fun}(k)       &= \sum\_{i=1}^k i^2.
\end{align\*}

Let&rsquo;s implement this in Python with an imperative style of writing:

```python
def fun(k):
    num = 0
    for i in range(k + 1):
        num += i**2
    return i
```

We&rsquo;re telling the computer here to step through all non-negative integers up to `k` and adding the square of these integers to a cumulative sum of all the previous numbers.
Perfectly intuitive and reasonable.

Let&rsquo;s implement this in Haskell, a more-or-less pure functional programming language with a highly expressive, declarative style of syntax:

```haskell
fun k = sum [i*i | i <- [0..k]]
```

This solution is elegant once you know that the `<-` syntax is the set containment symbol \\( \in \\) from mathematics.

This declarative style of writing code is something I find myself coming back to whenever I want to quickly hack an idea together.
It lets me focus on implementing the key steps of the problem I have laid out, without getting bogged down with more minor details of how it should be solved.
Focusing on the _what_ and not the _how_ is a fast-track to getting a compiled program that does what you want it to do quickly.

Of course, this type of programming is not without faults.
For one, you are leaving the &ldquo;how&rdquo; up to compiler with how it implements the expressive syntax.
Compiler engineers and developers for the languages are a clever bunch, but if you want more fine-grain control, the imperative style is superior.

Another drawback to the declarative style is working on large projects with many developers.
Since declarative programs tend to read closer to natural language, it&rsquo;s possible that each developer will craft their own dialect within the confines of the syntax.
Having several people with several dialects working on a single project can be a challenge.
Oftentimes for the sake of team cohesion and ease of debugging, a unified style-guide for large projects is preferred.

Despite this, I will continue to prefer declarative programming for quick implementations, only moving to imperative when I think more control is needed.
