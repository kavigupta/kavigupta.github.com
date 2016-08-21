---
layout: post
title: Introducing the Circle-Tree System
comments: True
---

If you want to follow along on paper, you'll want some colored pencils. Like at least 5 colors, not including a pencil to draw links.

# Circtrees

In any case, we'll call the diagrams we're interested in "circtrees."[^1] Circtrees can be constructed in one of three ways.

## 1. Dots

A dot of any color is a circtree. Here are some examples[^2]:

<center><img src="/circle-tree-system/intro/vars.svg"  /></center>

## 2. Bubbles

A circtree that's placed in a bubble is still a circtree. Note that you multiple bubbles around a circtree, since each time you bubble it, it's still a circtree.

<center><img src="/circle-tree-system/intro/lambdas.svg"  /></center>

(For now, the light coloration inside the circles is a little confusing; it'll make for nicer diagrams later. For now, just ignore the filling and focus on the bubble boundary.)

## 3. Links

A circtree that is linked to another circtree is a circtree.

<center><img src="/circle-tree-system/intro/apps.svg"  /></center>

The links can overall form a tree-like structure, for example:

<center><img src="/circle-tree-system/intro/tree.svg"  /></center>

You might have to flip the circtree upside down to see the tree, but it's there[^3].

## And that's all

Those are the only ways to create circtrees! It's a fairly simple system, but we can see that it's an incredibly useful one.

# Redraws

A redraw is a way to take a circtree and get another circtree. The intuition behind a redraw is that if we can redraw two circtrees into the same circtree, they are "equivalent" in some sense.

# The Recolor Redraw

A recolor is a way to change up the colors on a diagram. To understand the steps, we first need to define a bit of terminology: "contained".

## What does "Contained" mean?

Contained is a relationship between dots and bubbles of the same color. It doesn't make sense to say if a tree is contained in a bubble or if a dot is contained in a tree.

The basic intuition behind containment is that bubbles are kinda like fences that keep dots (let's call them sheep[^4]) of the same color in. So if you have:

<center><img src="/circle-tree-system/intro/red-inside-red.svg"  height="200" /></center>

then the red dot ("sheep") is contained inside the red bubble ("fence").

However, if you have:

<center><img src="/circle-tree-system/intro/red-inside-blue.svg"  height="200" /></center>

then the red sheep can escape because it can travel through a blue fence to the outside world. So we call this dot "free".

We say that a dot is "contained" by the first fence that it runs into if it tries to escape.

Now let's look at a more complicated example:

<center><img src="/circle-tree-system/intro/contained-threedots.svg"  height="200" /></center>

With the green dot, if we move outward, we hit the inner green bubble. Therefore the green dot is contained in the inner green bubble. The red dot can "move through" the green fence, but it can't get out of the red one immediately outside, so it is contained in the red fence.

Finally, the blue dot can go through both fences to freedom! That means that the blue dot is free.

<mark>Important note: When we talk about containment, we talk about where dots <i>could move</i> under the restriction that they can't move through bubbles of the same color, not where dots <i>do move</i>. Dots don't actually move through bubbles of different colors (i.e., moving a dot through a bubble isn't an allowable redraw in general); it's just a thought experiment to motivate this definition. </mark>

Here's another example:

<center><img src="/circle-tree-system/intro/contained-outside.svg"  height="200" /></center>

If we look at the dot on the left, it's clear that it's stuck in the bubble on the left. However, the dot on the right is free because it isn't in a bubble at all!

And finally one last example:

<center><img src="/circle-tree-system/intro/contained-two-same-color.svg"  height="200" /></center>

OK, so this one's a little more complicated. If we look at the dot on the left, if it wanted to escape it would have to go through *two* red fences. So which one is the one it's contained in? Well, the definition says that it's always the first fence that blocks it, so it's contained in the smaller bubble. The red dot on the right, however, is contained in the larger bubble because that's the first (and only) fence that would block *it*.

## Problems for Contained

For each of the given circtrees, go through every dot and figure out if it is contained in a bubble or if it is free. If it is contained in a bubble, identify the bubble.

<center><img src="/circle-tree-system/intro/contained-exercises.svg"  height="600" /></center>

## What is recoloring?

It's probably best to think of recoloring as a puzzle. You're shown a circtree, for example:

<center><img src="/circle-tree-system/intro/recolor-example.svg"  height="400" /></center>

And then you're given a bubble (for example, the larger green one) and told to change it to a color of your choice. You are allowed to change the color of any of the dots.

The one rule is that which bubble dots belong to should not be changed. However, actually just randomly picking colors for each dot until you find an arrangement that works takes a long time, so we instead have a few shortcut rules that lead to a process for recoloring rather than just a definition.

### 1. Contained Dots

Let's say we just change the color of the green bubble to yellow;

<center><img src="/circle-tree-system/intro/recolor-example-didnt-change-contained.svg"  /></center>

Why doesn't it work? Well, we can see that the green dots on the left and in the purple bubble are free whereas before they were contained in our bubble. What this tells us is that we need to change the colors of the dots that are contained in our bubble. So what happens when we change the colors of other dots?

<center><img src="/circle-tree-system/intro/recolor-example-changed-contained-in-other.svg"  /></center>

Here the issue is that the dot on the right goes from being contained in the small green bubble to being contained in the large formerly-green one. So, we need to change the colors of *only* the dots contained in our bubble.

<center><img src="/circle-tree-system/intro/recolor-example-correct-to-yellow.svg"  /></center>

OK, so that one worked! This leads us to the first derived rule: <mark>Dots contained in the given bubble must have their colors changed in the same way as the bubble. Other dots' colors are unchanged.</mark>

### 2. Locally Free Dots

So can we in general recolor a bubble and all the dots it contains to any color? Well, let's try a different color, say red:

<center><img src="/circle-tree-system/intro/recolor-example-capture-locally-free.svg"  /></center>

OK, so that one didn't work. Why? See the red dot on the far left? It was contained in the large red bubble and got captured by our recolored bubble. So, is the rule that we aren't allowed to pick colors of dots within the bubble that we are recoloring? Well, let's try changing the color to blue:

<center><img src="/circle-tree-system/intro/recolor-example-correct-to-blue.svg"  /></center>

If you look at each dot before and after, you can in fact verify that this transformation works! What is the difference between this and the last one? Well, in this case the blue dot doesn't get captured because it was already contained by the smaller blue bubble.

So we know that the problem only pops up when there are dots of our desired color that aren't contained by a bubble smaller than our target bubble. This leads us to the concept of a locally free dot: a dot that can escape the bubble we are considering. The fence-sheep intuition for this is that if we don't want to repaint our fence in such a way that sheep that could escape it before (locally free dots) aren't captured.

Using this definition, we have the second derived rule: <mark>The new color cannot be the color of any dots that are locally free to the given bubble.</mark>

### 3. Capturing Bubbles

But this isn't the only restriction: for example, there are no purple locally free dots, but this recoloring is still invalid:

<center><img src="/circle-tree-system/intro/recolor-captured-by-bubble.svg"  /></center>

Why is this a problem? Well, the dot on the right of the originally purple bubble was contained in the target bubble before but got captured by the smaller purple bubble. We call any bubble that physically surrounds a dot that is contained in our target bubble a "capturing bubble".

The fence-sheep intuition for this one is that we don't want to repaint any sheep to a color that means that they get captured by a smaller fence. We want to make sure that they stay blocked first by the fence we're currently repainting.

This definition leads us to the final rule: <mark>The new color cannot be the color of any capturing bubbles.</mark>

### How to Recolor

OK, so there's a lot of advice on how *not* to recolor, but how do we ensure that recoloring is a success? Well, as it turns out, if we follow the three rules I listed above, we will never break the same-contained-status rule. Given this, there's a simple strategy for recoloring:

> 1. Write down a list of colors for locally free dots
> 2. Write down a list of colors of capturing bubbles.
> 3. Find a color not on either of those lists.
> 4. Recolor the bubble and all dots it contains to the selected color

Of course, you can bypass steps 2 and 3 by just selecting a color that's not on the diagram already. While this always theoretically works, in practice you run out of marker colors (or colors that can easily be distinguished), so I recommend following steps 2 and 3 if your diagram has a lot of colors already.

## Exercises for Recoloring

For each image, determine whether or not the transformation is a valid recoloring.

<center><img src="/circle-tree-system/intro/recolor-questions.svg"  /></center>


# The Bubble Burst Redraw

Bubble bursting is the second basic redraw. A bubble burst can only be done when there is a link with a bubble on the left.

A bubble burst consists of "bursting" the bubble on the left by removing it and replacing all the dots it contains with the circtree from the right. Similar to redraw, we need to make sure that any locally free variables on the right aren't captured.

<center><img src="/circle-tree-system/intro/burst1.svg"  /></center>

For example, in the circtree above we can see that the red bubble is burst and the red dot is being replaced by the green dot. This is a valid recolor because the green dot is free both before and after the burst.

However, this burst is *not* valid:

<center><img src="/circle-tree-system/intro/burst-invalid1.svg"  /></center>

This is because the blue dot goes from being locally free to being captured.

## Examples

The circtree on the right of the link doesn't have to be a dot, it can be anything. For example, it can be a bubble:

<center><img src="/circle-tree-system/intro/burst2.svg"  /></center>

Or even a tree:

<center><img src="/circle-tree-system/intro/burst3.svg"  /></center>

## Multiple Step Redrawing

Generally, we want to burst bubbles. Bursting bubbles is basically the point of the circle-tree system. But sometimes colors can get in the way, as in the previous example. So what should we do? Recolor!

For example, we can burst the above bubble as follows:

<center><img src="/circle-tree-system/intro/reduce1.svg"  /></center>

## What is Bubble Bursting?

I mentioned above that Bubble Bursting is the point of the Circle-Tree system. Why is this? Well, bubble bursting is useful because it allows us to treat bubbles as templates that we can plug any circtree into. For example, let's say we have a common pattern, like so:

<center><img src="/circle-tree-system/intro/pattern-impl.svg"  /></center>

Through the bubble burst mechanism, each diagram can be restated:

<center><img src="/circle-tree-system/intro/pattern-abs.svg"  /></center>

And we can see that there is a common element, a circtree that represents a *pattern*:

<center><img src="/circle-tree-system/intro/pattern.svg"  height="200" /></center>

In this view of circtrees, bubbles represent a template, and linking a template to a value represent the "filling in" of the template with that value. You can actually fill in the template by bubble bursting.

# Bubble Creation

Bubble creation is the final basic redraw. A bubble creation can be done on any circtree. Basically, we take a circtree and link a dot on the right. Then we draw a bubble around it.

<center><img src="/circle-tree-system/intro/create1.svg"  /></center>

The one rule of bubble creation is that it isn't allowed to capture any free variables. So, this isn't valid:

<center><img src="/circle-tree-system/intro/create-invalid.svg"  /></center>

The basic idea behind bubble creation is that a bubble creation around a bubble can be undone by a burst:

<center><img src="/circle-tree-system/intro/create-burst.svg"  /></center>

So, really, the only reason to ever use this is if you want to show that two circtrees that represent the same template are in fact the same. This is generally a last step when showing that two circtrees are equivalent.

# And that's the system!

Yeah, that's it. Pretty simple, huh? OK, maybe it's not that simple. But when you compare it to what it's capable of, it starts looking pretty simple...

# Until next time...

Play around with this for a while, it really only makes sense when you have some experience manipulating the circtrees.


[^1]: You can probably guess where that name came from

[^2]: I choose to draw pretty big dots, but if you're doing this by hand, you'll probably want to use smaller ones for ease of drawing.

[^3]: CS people generally draw trees upside down. I guess because most of us have at best a passing familiarity with the biological equivalent.

[^4]: If you don't like sheep, then you can mentally substitute cows or pigs. But not chickens. They'd just fly over the fence.