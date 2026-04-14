preprocess:
    replace "IMAGE: ([^;\n]+);?(.*)" -> "<center><img src=\"/circle-tree-system/pair/\\1.svg\" \\2 /></center>"
    replace "<!--a*-->" -> ""
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

IMAGE: cons-first-second

We also need to make sure the following rules are followed:

IMAGE: pair-rules

## How to Make Pairs

So the construction circtree effectively takes in two things and jams them together somehow. How can we put multiple things together? Let's use a red and a blue dot as the example.

One way to combine two things might be to link them together: then we'd have

IMAGE: cons-is-app

However, I can't see a good way to separate them out again. For example, what if instead of a red dot, we had a bubble? Then the bursting process would burst the bubble on the left, making it impossible to recover them. Thus, we can't directly link them.

What if we were to link them both to some bubble (using a left link, since a right link would just be a bubble linked to what we have above).

IMAGE: cons-real

(Here, `S` is a "Selector" that picks with element). Then we could extract the first element by using two layers of bubbles and get the outer one, as such:

IMAGE: unwrapped-first-pair

Similarly, we could extract the second element by getting the inner one, as such:

IMAGE: unwrapped-second-pair

We can abstract each of these out into a common pattern:

IMAGE: fst-snd-of-cons

OK, so now we have a pair circtree for red and blue:

IMAGE: pair-x-z

And we can rewrite that as

IMAGE: cons-x-z

Meaning that we have

IMAGE: cons-defn

We can then say that

IMAGE: fst-snd-of-cons-abs

So now we have a way to construct and select from pairs

IMAGE: pair-impls

This gets us one step closer to circle-trees as an actual system for calculation! Next up, how to use pairs in a useful context.
