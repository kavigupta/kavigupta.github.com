---
layout: post
title: Who Needs Delta? Defining the Hyper-Real Numbers
comments: True
---

\\(\renewcommand{\ep}{\varepsilon}\\)
\\(\renewcommand{\d}{\mathrm d}\\)

In high school Calculus, solving differential equations often involves something like the following:

\\[\frac{\d y}{\d x} = \frac{y}{x}\\]

\\[\frac{\d y}{y} = \frac{\d x}{x}\\]

\\[\int \frac{\d y}{y} = \int \frac{\d x}{x}\\]

\\[\log y = c + \log x\\]

\\[y = kx\\]

Now, I don't know about anyone else, but this abuse of notation really bothered me at first. Everyone knows that \\(\d x\\) isn't an actual number by itself, but we seem to use it all the time as an independent variable.

<!--end excerpt-->

## Fluxions and Limits

Well, as it turns out, early mathematicians had the same problem with Newton's original formulation of Calculus, in which he called these infinitesimal quantities ["fluxions"](https://en.wikipedia.org/wiki/Method_of_Fluxions). His explanation was that they were quantities which "flowed" to 0 from before to after he took a derivative.

Obviously, in modern mathematics, this could not stand, and we got the following definition:

\\[\lim_{x \to a} f(x) = L \iff (\forall \ep > 0)(\exists \delta)(\|x - d\| < \delta \implies \|f(x) - L\| < \ep) \\]

\\[f'(x) = \lim_{h \to 0} \frac{f(x+h) - f(x)}{h} \\]

This system, known as \\(\ep--\delta\\), has been an important part of mathematics ever since it was defined.

I find this equation particularly elegant; it places the idea of limits and derivatives firmly in the language of set theory, and does so in a simple manner that, after some thought, is somewhat intuitive.

## Back to Infinitesimals

However, when we think about what the velocity of a particle in free fall, we generally don't think in terms of finding some \\(\delta\\) for every \\(\ep\\). I generally tend to find it easier to think in terms of a small change in position over a small change in time.

In fact, there is a way to formalize this relationship, found in the field of [non-standard analysis](https://en.wikipedia.org/wiki/Non-standard_analysis). To do so, we introduce an infinitesimal element, which is known as \\(\ep\\). This element has the property that

\\[(\forall n \in \mathbb{N^+})(0 < \ep < \frac{1}{n})\\]

We can therefore define addition and subtraction as simple vector addition and subtraction over \\(\\{1, \ep\\}\\) fairly simply, in a similar manner to complex addition and subtraction.

## \\(\ep^2\\)
Multiplication is a little more complicated. We know that

\\[(a + b\ep)(c + d\ep) = ac + (ad + bc)\ep + bd \ep^2\\]

but what should \\(\ep^2\\) be? If we decide to keep the result within our new vector-like field, we end up with the equation \\(\ep^2 = a\ep + b\\), for real \\(a, b\\). I'll spare you the [derivation](https://en.wikipedia.org/wiki/Hypercomplex_number#Two-dimensional_real_algebras), but it turns out that there are only three different systems we can get out of this formula. Two are useless for our purposes, being the complex numbers and something called the split-complex numbers, in which there is some \\(j^2 = 1, j \neq \pm 1\\). The last, the Dual Numbers, is a system in which \\(\ep^2 = 0\\).


## Dual Numbers

Dual Numbers at first seem quite promising. The idea that \\(\d x^2 = 0\\) sounds pretty similar to the notion of higher order differentials vanishing to 0. However, there are a few problems. First of all, the second derivative \\(\frac{\d^2 x}{\d x^2}\\) contains second degree differentials on both the numerator and the denominator, which means that it would vanish to 0.

Secondly, and more disturbingly, \\(\ep^{-1}\\) is not defined in the dual numbers. This is because of the following. Define \\(\omega = \ep ^ {-1}\\). Then

\\[\omega\ep = 1\\]
\\[\omega^2\ep^2 = 1\\]
\\[\omega^2 0 = 1\\]

Which is an obvious contradiction. Therefore, the Dual Numbers are not a field and therefore not what we are looking for.

## Full System of Infinitesimals

Obviously, we need to define \\(\ep^2\\). So let's just define it as \\(\ep^2\\). Same for \\(\ep^3, \ep^4, \ep^5, \ldots\\). We also, by fiat, define \\(\ep^{-1} = \omega\\). This of course introduces \\(\omega^2, \omega^3, \omega^4,\ldots\\).

This set is called the [Hyperreals](https://en.wikipedia.org/wiki/Hyperreal_number).

Now, it might at first appear that we have infinitesimals corresponding to every integer. Some Haskell programmers out there might be thinking in terms of infinite lists (I know that was _my_ first thought at least). However, here's a problem: what's \\(\sqrt{\ep}\\)?

## Level comparison

Before we can fully analyze what \\(\sqrt{\ep}\\) means, we should define some notion of comparability. For example, we might want to define some notion of something being infinitesimal compared to something else. We can define \\(\ll\\), read "infinitesimal compared to" as follows:

\\[x \ll 1 \iff (\forall n \in \mathbb{N})(|x| < \frac{1}{n})\\]
\\[x \ll y \iff y \neq 0 \wedge \frac{x}{y} \ll 1\\]

Note that we decided to use the absolute value. Then, we can say \\(0 \ll -1\\). This is to allow for negative numbers to be compared in this way; the sign is simply ignored.

We can also define the comparators \\(\gg\\) and \\(\bot\\), read "infinite compared to" and "comparable to" as:

\\[x \\gg y \iff y \\ll x\\]
\\[x \bot y \iff \neg (x \ll y) \wedge \neg (x \gg y)\\]

The following proofs are left as an exercise to the reader:

1. Exactly one of \\(x \gg y\\), \\(x \ll y\\), and \\(x \bot y\\) is true
2. \\(\ll\\) is transitive.
3. \\(x \ll y \iff kx \ll ky\\) for every \\(k \neq 0\\)

## All powers of infinitesimals

Now we can analyze what \\(\sqrt{\ep}\\) means. We know that since \\(\ep \ll 1\\), \\(\\sqrt{\ep} \ll 1\\). We also know that since \\(\frac{\ep}{\sqrt{\ep}} = \sqrt{\ep} \ll 1\\), \\(\ep \ll \sqrt{\ep}\\). We now know that

\\[\ep \ll \sqrt\ep \ll 1\\]

This means that \\(\sqrt\ep\\) is on a separate level of infinitesimal than any of the integral powers of \\(\ep\\). Since similar proofs can be constructed for all rational powers, and therefore for all real powers (yes, we have to go back to \\(\ep---\delta\\) for this), the hyper-reals therefore have as many levels as there are real numbers.

## Derivatives

OK, so I know I promised you at the beginning of this section that this would all eventually lead to a more intuitive approach to derivatives. Unfortunately, however, this is far too long of a post already. I'll cover derivatives in the next section.


**Editorial Note:** A previous version of this post had a few integrals where they should not have been. It has been corrected.
