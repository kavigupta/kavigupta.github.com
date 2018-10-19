---
layout: post
title: Storing Data With the Circle-Tree
comments: True
---

## The Pair Abstraction

Computers need some way to store information. More specifically, we want to be able to store multiple pieces of data in one place so that we can retrieve it later. For now, we'll look at the simplest way of storing data, a pair.

To express the concept of a "pair", we need three machines.

 1. A way to put two things together (construction, or "C" for short)
 2. A way to get the first element (first, or "F" for short)
 3. A way to get the second element (second, or "S" for short)

 <!--end excerpt-->

We will represent these for now as labeled dots (we'll show how to make them later).

<center><img src="/circle-tree-system/pair/cons-first-second.svg"  /></center>

We also need to make sure the following rules are followed:

<center><img src="/circle-tree-system/pair/pair-rules.svg"  /></center>

## How to Make Pairs

So the construction circtree effectively takes in two things and jams them together somehow. How can we put multiple things together? Let's use a red and a blue dot as the example.

One way to combine two things might be to link them together: then we'd have

<center><img src="/circle-tree-system/pair/cons-is-app.svg"  /></center>

However, I can't see a good way to separate them out again. For example, what if instead of a red dot, we had a bubble? Then the bursting process would burst the bubble on the left, making it impossible to recover them. Thus, we can't directly link them.

What if we were to link them both to some bubble (using a left link, since a right link would just be a bubble linked to what we have above).

<center><img src="/circle-tree-system/pair/cons-real.svg"  /></center>

(Here, `S` is a "Selector" that picks with element). Then we could extract the first element by using two layers of bubbles and get the outer one, as such:

<center><img src="/circle-tree-system/pair/unwrapped-first-pair.svg"  /></center>

Similarly, we could extract the second element by getting the inner one, as such:

<center><img src="/circle-tree-system/pair/unwrapped-second-pair.svg"  /></center>

We can abstract each of these out into a common pattern:

<center><img src="/circle-tree-system/pair/fst-snd-of-cons.svg"  /></center>

OK, so now we have a pair circtree for red and blue:

<center><img src="/circle-tree-system/pair/pair-x-z.svg"  /></center>

And we can rewrite that as

<center><img src="/circle-tree-system/pair/cons-x-z.svg"  /></center>

Meaning that we have

<center><img src="/circle-tree-system/pair/cons-defn.svg"  /></center>

We can then say that

<center><img src="/circle-tree-system/pair/fst-snd-of-cons-abs.svg"  /></center>

So now we have a way to construct and select from pairs

<center><img src="/circle-tree-system/pair/pair-impls.svg"  /></center>

This gets us one step closer to circle-trees as an actual system for calculation! Next up, how to use pairs in a useful context.
