---
layout: post
title: What is Category Theory?
comments: True
---

\\(\newcommand{\mr}{\mathrm}\\)
So, I was reading [_Category Theory for Scientists_](http://math.mit.edu/~dspivak/teaching/sp13/) recently and kept getting the question: What _is_ category theory? Well, until I hit chapter 3-4, I didn't really have an answer. But now I do, and I realize that to just understand what it is doesn't really require understanding how it works.

## Sets and functions

So, set theory is all about sets and functions. Why are we talking about set theory and not category theory, you may ask? I'll get to category theory eventually, but for now, let's discuss something a little more concrete.

Let's look at subsets of the set \\(\\{a, b\\}\\). We have the possibilities, which we name <b>E</b>mpty, <b>A</b> set, <b>B</b> set, <b>U</b>niversal set:

\\[E = \\{\\}, A = \\{a\\}, B = \\{b\\}, U = \\{a, b\\}\\]

<img src="/resources/2016-01-28/sets.svg.png"/>

We can then look at the functions between the sets; here expressed as a set of tuples, \\(f = \\{(x, fx)\\}\\) where there is one element for every element of \\(x\\).

Between \\(E\\) and any set \\(x\\), we have the function \\(ab_x = \{\}\\), which needs no mappings. (This is called the `absurd` mapping because if you have to use this function, you are in an absurd situation, a function with no potential arguments).

<img src="/resources/2016-01-28/absurd.svg.png"/>

Now, we can add the functions from the one-element sets. We know that there is no function to the empty set, since all functions need to have an output for every input. The functions from the one element sets to the other sets are all the constant function \\(s_x\\) mapping the only element of the domain to the specified element \\(x\\).

<img src="/resources/2016-01-28/single.svg.png"/>

## Abstractions in Mathematics

Abstractions are a nice way to be able to look at certain properties of a mathematical structure without understanding how it "works" internally.

### Example: Pair

An example, taken from computer science, is the pair. You can think of a pair as one thing \\(p\\) that contains two things \\(a\\) and \\(b\\). To formalize this definition, we can say that a pair is a triple of _functions_ \\((\mr{pair}, \mr{first}, \mr{second})\\) such that

\\[\mr{first}(\mr{pair}(a, b)) = a\\]
\\[\mr{second}(\mr{pair}(a, b)) = b\\]

Pairs can then be represented in many ways. We can represent a pair of integers, for example, as:

\\[\mr{pair}(a,b) = 2^a3^b\\]
\\[\mr{first}(p) = \nu_2 p\\]
\\[\mr{second}(p) = \nu_3 p\\]

where \\(\nu_p n\\) is the function returning the highest power of the prime \\(p\\) in \\(n\\).

We can also represent pairs as higher-order functions:

\\[\mr{pair}(a, b) = f \mapsto f(a, b)\\]
\\[\mr{first}(p) = p(\(a, b) \mapsto a)\\]
\\[\mr{second}(p) = p(\(a, b) \mapsto b)\\]

However, pairs aren't a particularly interesting example. No matter how we represent a pair, a pair is just a pair. We don't treat a church pair (the second example) as a function, we just use the \\(\mr{first}\\) and \\(\mr{second}\\) functions on it.
