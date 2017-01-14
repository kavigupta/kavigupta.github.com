---
layout: post
title: Circle-Tree as a Calculator
comments: True
---


# Using Circle-Tree Drawings as a Calculator

So this isn't just a procrastination game for doodling when you're bored (and have a lot of colored pencils, for some reason). We can use circle-tree as a way to express math!

## Numbers

For math, the first thing we need are numbers. I'm just going to draw some pictures which are *defined* to be the numbers. We don't worry too much about whether they are "really" numbers or not in the way that we don't worry about whether "102" or "CII" is a better way to explain the concept of "how many cents in a dollar plus how many hands a person has"ness.

So this is 0:

<img src="/resources/2016-07-29/0.svg" />

And this is 1:

<img src="/resources/2016-07-29/1.svg" />

And this is 2:

<img src="/resources/2016-07-29/2.svg" />

And this is 3:

<img src="/resources/2016-07-29/3.svg" />

I hope you see the pattern, but to drive it home, I'll jump to 13:

<img src="/resources/2016-07-29/13.svg" />

You can see that in every case, the number of "red dots" (more precisely, dots contained in the outer bubble) is equal to the number we're trying to write. Additionally, you can see that there's always that one "green dot" (dot contained in the inner bubble) on the far right, with all links "leaning right" (each red dot is connected to a link which represents the rest of the dots).

## Addition

Well, numbers aren't that useful if you can't do math with them! (I mean, that's the real reason that we use 102 and not CII. It's hard to do math when you have no concept of place-value.)

So, how do we define addition? Well, let's look at a picture:

<img src="/resources/2016-07-29/plus.svg" />

Yeah, so it's not a model of simplicity. But it's incredibly powerful. Let's link it to, say, 5 and 3:

<img src="/resources/2016-07-29/plus-5-3.svg" />

Now you should immediately see that some bubbles can be burst here:

<img src="/resources/2016-07-29/plus-5-3-red1.svg" />

And then can burst some more bubbles!

<img src="/resources/2016-07-29/plus-5-3-red2.svg" />

You get the picture.

<img src="/resources/2016-07-29/plus-5-3-red3.svg" />

And now we have the result, 8. Now, you can probably see how the addition works: it "adds" two numbers in the way that if you have 3 things and you have 5 things, and you put them next to each other, you have 8 things. OK, so maybe *powerful* wasn't exactly the best way of putting it. But addition's not all we can do.

## Multiplication

So how can we multiply two things together? Let's answer a simpler question first: how do we multiply something by 4? Well, let's look at something even simpler: how would we add 2 + 2 + 2 + 2? Let's try to avoid having 3 additions, since that's kinda complicated. We can skip to the second-to-last step, which we can still see will add the values:

<img src="/resources/2016-07-29/plus-2-2-2-2.svg" />

So you can kinda pop those bubbles in your head, and you'll get 8. But repeating the same thing four times is kinda preventing us from seeing the bigger picture. How do we get rid of repeated elements? Well, we can *create* repeated elements by bursting a bubble, so if we can walk that process back, we get:

<img src="/resources/2016-07-29/plus-2-2-2-2-abs.svg" />

Looking at the left bubble, we can see that it is in fact 4! And on the right, we have something that's *alomst* 2, except that it's missing the outer layer and it includes a free red dot. We can fix both problems by drawing a bubble around it that's immediately burst.

<img src="/resources/2016-07-29/plus-2-2-2-2-abs2.svg" />

And now we want a picture that's kinda like the addition one: when linked to two numbers, it represents the product of the two numbers. We can again do this by blowing two bubbles, the outer one representing the first factor and the inner one representing the second:

<img src="/resources/2016-07-29/plus-2-2-2-2-abs3.svg" />

We now have a multiplier! It works because bubble bursting allows us to duplicate something multiple times, and numbers are effectively machines to duplicate stuff. For example, if you burst 10, you repeat whatever it was linked to 10 times.

## Exponentiation

So we can add, and we can multiply. What's next? Exponentiation! We can represent exponentiation as repeated multiplication. Again, we skip to a few steps from the end of the bubble-burst process (or a few steps from the beginning of the bubble creation process). Using the example 2 * 2 * 2 * 2, we have the bubble:

<img src="/resources/2016-07-29/mult-2-2-2-2.svg" />

If you go through the process, you can see that the result is in fact \\(2^4 = 16\\). We can create a bubble and move the 2s out:

<img src="/resources/2016-07-29/mult-2-2-2-2-abs.svg" />
