---
layout: post
title: A Monadic Model for Set Theory
comments: True
---

## Solving an Equation

A lot of elementary algebra involves solving equations. Generally, mathematicians
seek solutions to equations of the form

\\[f(x) = b\\]

The solution that most mathematicians come up with to this problem is to define
left and right inverses: The left inverse of a function \\(f\\) is a function \\(g\\)
such that \\(g \circ f = \mathrm {id}\\). This inverse applies to any function that
is one-to-one. There is also a right inverse, a function \\(g\\) such that
\\(f \circ g = \mathrm {id}\\) For example, the left inverse of the function

\\[x \mapsto \sqrt x :: \mathbb R^+ \rightarrow \mathbb R\\]

is the function 

\\[x \mapsto x^2 :: \mathbb R \rightarrow \mathbb R^+\\]

Because

\\[(\sqrt x)^2 = x\\]

On the other hand,

\\[\sqrt {x^2} = |x| \not\equiv x \\]

so it is not a right inverse.

Of course, this is a very function-oriented way of looking at the problem. Nothing
wrong with that, of course, it [actually generalizes pretty well](https://kavigupta.github.io/2016/01/28/What-Is-Category-Theory/),
but it can sometimes seem like it's getting in the way.
