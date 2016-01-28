---
layout: post
title: What is Category Theory?
comments: True
---

\\(\newcommand{\mr}{\mathrm}\\)
So, I was reading [_Category Theory for Scientists_](http://math.mit.edu/~dspivak/teaching/sp13/) recently and kept getting the question: What _is_ category theory? Well, until I hit chapter 3-4, I didn't really have an answer. But now I do, and I realize that to just understand what it is doesn't really require understanding how it works.

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
