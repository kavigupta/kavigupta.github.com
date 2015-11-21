---
layout: post
title: Ancient Greek Abstract Algebra&#58; Axioms
comments: True
---

$$\newcommand{\bo}{\mathbf}$$

## Logic

I will be using standard propositional logic. At first, I will write both the verbal (e.g., if $$p$$ implies $$q$$, not $$q$$ implies not $$p$$) and the symbolic forms (e.g., $$(p \implies q) \implies (\lnot q \implies \lnot p)$$). Later, however I will begin to drop the verbal forms. For reference, see [this Wikipedia post on propositional logic notation](https://en.wikipedia.org/wiki/List_of_logic_symbols)

## Undefined concepts

The concept of a point is undefined and treated as a primitive notion.

The concept of a shape is undefined and roughly corresponds to a set in set theory which can only contain points.

## Notation

- Points are notated with upper case letters (e.g., $$P$$, $$Q$$, $$R$$)
- Shapes are notated with bold letters (e.g., $$\bo C$$, $$\bo L$$)
- Shape constructors are denoted with a lower case Greek letter (e.g., $$\gamma, \lambda$$ followed by a parenthesized list of values)
- A point $$P$$ being in a shape $$\bo{X}$$ is denoted $$P \in \bo{X}$$
- Any symbol may be subscripted to distinguish it from other symbols. The subscripts, though they may be numeric, do not represent actual numbers, and can be considered to simply be a convenient way to extend the number of symbols.

## Axioms

### 1. Equality of Shapes

Two shapes $$\bo{S_1}$$ and $$\bo{S_2}$$ are equal if for all points $$P$$, $$P \in \bo{S_1}$$ if and only if $$P \in \bo{S_2}$$

\\[\bo{S_1} = \bo{S_2} \iff (\forall P)(P \in \bo{S_1} \iff P \in \bo{S_2})\\]

### 2. Definition of a Line

#### Construction of a Line
For all pairs of points $$P$$, $$Q$$ there exists a line
$$\bo{L} = \lambda(P, Q)$$ such that $$P \in \bo{L}$$ and $$Q \in \bo{L}$$
\\[(\forall P, Q)(\exists \bo{L} = \lambda(P, Q))(P\in \bo{L} \wedge Q \in \bo{L})\\]

#### Existence of Points on Line
\\[(\forall \bo L = \lambda(A,B))(\exists P, Q)(P \in \bo L \wedge Q \in \bo L)\\]
Given a line $$\bo{L}$$, there exist $$P$$ and $$Q$$ such that $$P \in \bo L$$, $$Q \in \bo L$$, and $$P \neq Q$$

### 3. Definition of a Plane

#### Construction of a Plane
For all triples of points $$P$$, $$Q$$, and $$R$$, there exists some plane $$\bo X$$, such that $$P \in\bo X$$, $$Q \in\bo X$$, and $$R \in\bo X$$.
\\[(\forall P, Q, R)(\exists \bo X = \phi(A,B,C))(P \in\bo X \wedge Q \in\bo X \wedge R \in\bo X)\\]

#### Choice of Points on Plane
For all planes $$\bo X$$ there exist some points $$P$$, $$Q$$, and $$R$$ such that $$P \in\bo X$$, $$Q \in\bo X$$, $$R \in\bo X$$, $$P \neq Q$$, and $$Q \neq R$$.
\\[(\forall \bo X = \phi(A, B, C))(\exists P, Q, R)(P \in \bo X \wedge Q \in\bo X \wedge R \in\bo X \wedge P \neq Q \wedge Q \neq R)\\]
