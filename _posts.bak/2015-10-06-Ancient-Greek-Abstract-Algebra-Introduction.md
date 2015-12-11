---
layout: post
title: Ancient Greek Abstract Algebra&#58; Introduction
comments: True
---

Note: I've put the Ancient Greek Abstract Algebra series on hold for now. Please take a look at some of my other post(s).

## Modern constructions of $$\mathbb{N}$$

The natural numbers are taught to children as a basic facet of mathematics. One of the first things we learn is how to count. In fact, the name "Natural Numbers" itself implies that the natural numbers are basic.

However, modern mathematicians seem to have trouble with accepting the natural numbers as first class features of mathematics. In fact, there are not one but two common definitions of the naturals that can be used to prove the Peano Axioms (the basic axioms from which most of number theory can be proven).


### Church Numerals

\\[0 = f \mapsto (t \mapsto t)\\]

\\[S(n) = f \mapsto (f \circ n(f))\\]

In this way, the church numeral $$n$$ is defined as the function that takes a function and outputs the $$n\th$$ repeated application of it.

### Set Theory Numerals

\\[0 = \\{\\{\\}\\}\\]
\\[S(n) = n \cup \\{n\\}\\]

The set theoretic numerals are therefore defined as the set of all the elements before them along with the empty set.

## Ancient Greek Abstract Algebra

A modern mathematician would justify the definition of the natural numbers as a way to base an inherently complicated system with an infinite number of elements on a far simpler system containing only a few axioms.

However, modern mathematicians' discomfort with numbers and desire for algebraic constructions, to the uninitiated, can often seem bizarre. In fact, my first thought when I learned about the construction of the natural numbers was to think of another type of construction: Euclid's compass and straightedge.

Personally, I like modern algebra, but I don't like geometry. Whenever I see a problem in geometry, I immediately try to transform it into an algebraic problem by introducing coordinates. I used to think that I didn't like geometry because it is unsystematic. However, I have come to realize that a large part of my aversion has to do with language.

## Language

Algebra has an important modern advantage over geometry: a symbolic language. For example, the following is the converse of the Pythagorean theorem, quoted from [Professor D.E. Joyce's page at Clark University ](http://aleph0.clarku.edu/~djoyce/java/elements/bookI/propI48.html):

> If in a triangle the square on one of the sides equals the sum of the squares on the remaining two sides of the triangle, then the angle contained by the remaining two sides of the triangle is right.

In symbols, this statement would be

\\[AC^2 = AB^2 + BC^2 \implies AB \perp BC\\]

I will say, and I think without the guilt of modern arrogance, that this formulation is much simpler. While it does require a little more background knowledge than the sentence form (one must know what the symbols represent), it is much shorter and can generalize to more than three variable quantities easily.

For example, try putting the quadratic formula $$ax^2 + bx + c = 0 \implies x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}$$ in a language with no concept of symbolic operations or variable names. The simplest result I can think of (with $$c$$ flipped to the other side to avoid saying "zero") is:

> If the sum of the the product of the square of a number and some coefficient and the number and some other coefficient is equal to some value, the number is equal to the quotient between the difference of a number whose square is equal to the sum between the square of the coefficient of the number and four times the product of the coefficient of the square of the number and the value and the coefficient of the number and twice the coefficient of the square of the number.

I can conclude with some confidence that putting Euclidean geometry into the language of abstract mathematics will make it more manageable.

Hey, it might even break down anti-geometry prejudices of people like me.
