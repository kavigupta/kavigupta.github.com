---
layout: post
title: Circle-Tree as a Calculator
comments: True
---


# Using Circle-Tree Drawings as a Calculator

So this isn't just a procrastination game for doodling when you're bored (and have a lot of colored pencils, for some reason). We can use circle-tree as a way to express math!

## Numbers

For math, the first thing we need are numbers. I'm just going to draw some pictures which are *defined* to be the numbers. We don't worry too much about whether they are "really" numbers or not in the way that we don't worry about whether "102" or "CII" is a better way to explain the concept of "how many cents in a dollar plus how many hands a person has"ness.[^1]

So this is 0:

<center><img src="/circle-tree-system/calc/0.svg"  /></center>

<!--end excerpt-->

And this is 1:

<center><img src="/circle-tree-system/calc/1.svg"  /></center>

And this is 2:

<center><img src="/circle-tree-system/calc/2.svg"  /></center>

And this is 3:

<center><img src="/circle-tree-system/calc/3.svg"  /></center>

I hope you see the pattern, but to drive it home, I'll jump to 13:

<center><img src="/circle-tree-system/calc/13.svg"  /></center>

You can see that in every case, the number of "red dots" (more precisely, dots contained in the outer bubble) is equal to the number we're trying to write. Additionally, you can see that there's always that one "green dot" (dot contained in the inner bubble) on the far right, with all links "leaning right" (each red dot is connected to a link which represents the rest of the dots).

## Addition

Well, numbers aren't that useful if you can't do math with them! (I mean, that's the real reason that we use 102 and not CII. It's hard to do math when you have no concept of place-value.)

So, how do we define addition? Well, let's look at a picture:

<center><img src="/circle-tree-system/calc/plus.svg"  /></center>

Yeah, so it's not a model of simplicity. But it's incredibly powerful. Let's link it to, say, 5 and 3:

<center><img src="/circle-tree-system/calc/plus-5-3.svg"  /></center>

Now you should immediately see that some bubbles can be burst here:

<center><img src="/circle-tree-system/calc/plus-5-3-red1.svg"  /></center>

And then can burst some more bubbles!

<center><img src="/circle-tree-system/calc/plus-5-3-red2.svg"  /></center>

You get the picture.

<center><img src="/circle-tree-system/calc/plus-5-3-red3.svg"  /></center>

And now we have the result, 8. Now, you can probably see how the addition works: it "adds" two numbers in the way that if you have 3 things and you have 5 things, and you put them next to each other, you have 8 things. So that's not the most intelligent way to add. But addition's not all we can do.

## Multiplication

So how can we multiply two things together? Let's answer a simpler question first: how do we multiply something by 4? Well, let's look at something even simpler: how would we add 2 + 2 + 2 + 2? Let's try to avoid having 3 additions, since that's kinda complicated. We can skip to the second-to-last step, which we can still see will add the values:

<center><img src="/circle-tree-system/calc/plus-2-2-2-2.svg"  /></center>

So you can kinda pop those bubbles in your head, and you'll get 8. But repeating the same thing four times is kinda preventing us from seeing the bigger picture. How do we get rid of repeated elements? Well, we can *create* repeated elements by bursting a bubble, so if we can walk that process back, we get:

<center><img src="/circle-tree-system/calc/plus-2-2-2-2-abs.svg"  /></center>

Looking at the left bubble, we can see that it is in fact 4! And on the right, we have something that's *alomst* 2, except that it's missing the outer layer and it includes a free red dot. We can fix both problems by drawing a bubble around it that's immediately burst.

<center><img src="/circle-tree-system/calc/plus-2-2-2-2-abs2.svg"  /></center>

And now we want a picture that's kinda like the addition one: when linked to two numbers, it represents the product of the two numbers. We can again do this by blowing two bubbles, the outer one representing the first factor and the inner one representing the second:

<center><img src="/circle-tree-system/calc/plus-2-2-2-2-abs3.svg"  /></center>

We now have a multiplier! It works because bubble bursting allows us to duplicate something multiple times, and numbers are effectively machines to duplicate stuff. For example, if you burst 10, you repeat whatever it was linked to 10 times.

## Exponentiation

So we can add, and we can multiply. What's next? Exponentiation! We can represent exponentiation as repeated multiplication. Again, we skip to a few steps from the end of the bubble-burst process (or a few steps from the beginning of the bubble creation process). Using the example 2 * 2 * 2 * 2, we have the bubble:

<center><img src="/circle-tree-system/calc/mult-2-2-2-2.svg"  /></center>

If you go through the process, you can see that the result is in fact \\(2^4 = 16\\). We can create a bubble and move the 2s out:

<center><img src="/circle-tree-system/calc/mult-2-2-2-2-abs.svg"  /></center>

Notice that this is 4 followed by 2. Interestingly, we are treating the number on the left as a machine, which is burst to provide the pattern for multiplying the number on its right by itself. However, if we generalize the pattern into a bubble, we need to draw two bubbles to burst the two numbers into. It ends up looking like this:

<center><img src="/circle-tree-system/calc/exp-2-4.svg"  /></center>

## Conclusion

So now you know how to do math using the circle-tree system. We can use certain bubbles as "machines" that allow us to perform certain actions. We can also use certain other bubbles as numbers. Even those bubbles are, at the end of the day, numbers, to repeat a bubble some number of times.

Basically, the circle-tree gives us a way to show how to calculate things. And we can use this as a way to reason about calculation itself.

[^1]: Although, off the record, for doing math CII is a *terrible* way of representing 102.
