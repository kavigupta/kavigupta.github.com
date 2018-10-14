---
layout: post
title: Simulationism&#58; Beyond the Matrix
comments: True
---

Simulationism is a fairly popular concept. There are many who subscribe to it: from those who use it as a thought experiment to argue against undisprovable theories to those who actually believe we are in a simulation since almost all universes should be simulations. Despite the disparate uses for simulationism, it is generally viewed as a fairly banal concept: the universe is a simulation, and that means absolutely nothing, practically speaking.

However, lurking beneath the seeming simplicity of the idea lies a far more radical set of implications.

<!-- end excerpt -->

## Pure simulationism vs the Matrix

Simulationism is often explained as such: "we're all living in a simulation, like the Matrix." For most purposes, this is a good working definition. Mostly, I've seen simulationism used as kind of the ultimate [Russell's Teapot](https://en.wikipedia.org/wiki/Russell%27s_teapot): an entirely undisprovable concept. As a Teapot, simulationism is a useful concept because it posits an alternate picture of the world that has no observable differences from ours: this property is true of the Matrix as well (well, apart from the glitches, and the actual plot of the movie).

On the other hand, there's an important difference between simulationism and the Matrix. In the Matrix, when you wake up, you are *you*: a real human conciousness produced by a meat brain whose sensory inputs are being manipulated to place you into an alternate reality. However, in simulationism, you can't wake up, because there is no underlying *you*: your entire existence, conciousness and sensory experience alike are the interaction between bits of information held in computer memory.

## All is information

This difference is fundamental: simulationism presupposes a philosophical concept that the Matrix does not: all that reality is can be reduced to information. This isn't quite materialism (in fact, it's listed as one of the criticisms of materialism on [materialism's Wikipedia article](https://en.wikipedia.org/wiki/Materialism#Quantum_mysticism)[^1]) but it is similar in its reductionist approach. To many, this seems like a pedantic point: this assumption is an easy one to make (unless you believe in a soul or other such concepts).

## What is a simulation?

So let's say our universe is indistinguishable from a simulation. What is a simulation? Well, in a conventional sense, a simulation is a program running on data. The program is some set of instructions that allows us to go from one state to another. Take the following very simple world which exists in one of two states: 0 or 1. There are only two possible sets of laws for this universe: the laws that keep it steady or the laws that cause it to flip back and forth. What about `1, 1, 0, 1, 1, 0, ...`, you might ask? Well, in that case the first and second 1s are different, which means that there is some additional state that the universe isn't taking into account.[^2]

For even more complicated universes, we'd want to be able to write down some mathematical formula or algorithm so we could actually run a simulation. It turns out that this isn't always possible for infinite universes (since the laws might take infinite space to write), but let's assume the universe is very large but not infinite and thus the rules can be written down.

Now some of you complexity fans out there might be thinking "wait, wait, wait, every set of laws of the universe might be expressible but only some of them would be quickly computable on a concise representation of the universe's state". Well, that's true. But also, does it really matter how slow the simulation runs?[^3] The concept of time is a product of the laws of the universe: it really shouldn't matter how long a second inside the universe takes outside the universe.

## Is any running computer a universe?

One of the more interesting things about simulationism is that it allows us to imagine ourselves creating a universe. If we created a powerful enough computer and figured out close enough approximations to the laws and initial state of our own universe, we could set up our own simulation of a universe similar to ours.

But let's say we didn't bother with any of that. Let's say we created a significantly simpler universe than our own on hardware we have sitting on our desk anyway. Is that simulation itself a universe? More generally, any running computer with no input or output from the outside world is a simulation of *something*, right? It has some state and rules for how to get from one state to another, which was our only definition of a universe.

Is any running computer a universe? Well, we seem to be backed into a corner: the answer has to be yes. One consoling fact is that the universe created by a computer running a screensaver would be nothing like our universe, it would be so dissimilar as to even make such a comparison quite a stretch.

## Do we need the computer?

Let's say I tell you about a simulation I have in mind: say the state is a number from 0 to 10 (starting at 1) and the law of the universe can be described as the function \\[f(n) = 2n \mod 11 \\] You could write a program to simulate this universe: you'd get a cycle of length 10 that went through every state but 0. But that cycle was fixed before you started running the program. Isn't the mere existence of the program then enough to set up the universe? More concretely, the function \\[g(n) = 2n \mod 4384165182867240584805930970951575013697 \\] (credit to [UTM](https://primes.utm.edu/lists/small/small.html) for the prime) has more or less the same behavior, though a much longer cycle, which we know even without running the simulation (which would take \\(5000\\) times longer than the age of the universe to compute on my desk computer). Does running the simulation even matter if writing it down plans its entire history out anyway?[^4]

So now, a program to calculate the next state, and a first state, written down together comprise a universe.

## Do we need to write it down?

But now we're in a weird spot: do we need to write the rules of the universe down? I'm not sure what the magic is of writing anything down, really. It's strange to imagine that a simulation of our universe begins to exist the moment we write it down. The mathematical object that is the universe certainly does not begin to exist: and at this point, what's the difference between the universe and its representation?

Of course, we've gotten to a truly ridiculous point: what this implies is that our universe could be a simulation, not running in some containing universe's computer, not written down on the page of some containing universe's book, but simply because it is mathematically possible. Similarly, every other universe with a slight modification of the laws of physics also exists in some other universe. Even more strangely, the universe identical to ours except that on January 1st, 2019, all blue objects turn green exists.[^5]

## If we're in a simulation, then all things are happening

Simulationism, with a few additional assumptions I've made along the way, leads inevitably to a really strong version of multiple universe theory: anything that can be mathematically described exists in some real world. More relevant to our current universe, if every possible set of laws is happening, *we don't know that our universe has parsimonious laws*. That means that we are equally likely to be in the "all laws normal" universe as we are to be in the "all blue objects turn green" universe. Thus, allowing for the premise of simulationism implies that scientific induction is literally impossible. The universe looks like it has laws simply by chance, and that fact could change at any minute.

One reason I think a lot of people are relatively comfortable with simulationism is because it seems relatively limited in its implications. In its simplest form, it's a creator allegory, with the simulation's programmers taking the place of a more conventional god figure. If thought through to its conclusion, it really is a far more radical idea, with jarring results.

[^1]: For some unfathomable reason under "Quantum mysticism"???

[^2]: The preceeding description, is missing something: what if the simulation isn't deterministic but is instead random? In that case, we have more possible sets of laws. For example, the next state of the universe could be decided by a coin flip. Or it could be decided by a coin flip with a coin that lands heads only 1/4 of the time, or it could be a combination of the current state and a coin flip. In fact, while there are now an infinite number of laws, we can still characterizing all of them with 4 numbers, that is the probability of staying at 0, staying at 1, moving from 1 to 0, and moving from 0 to 1. The need to have a way to generate randomness, e.g., a coin or a qubit, will be an issue in the future, but for now just assume the computer has access to such a source of randomness.

[^3]: An interesting thought experiment positing vastly different timescales inside and outside the simulation, as well as a nonstandard form of computation, is given by Randall Munroe in a classic [xkcd](https://xkcd.com/505/)

[^4]: Okay, so I ignored something here, which is the possibility of a nondeterministic program, which produces a different result every time it's run. However, the only additional assumption we need here is the existence of one standard random string of infinite length (known as the [Common reference string model](https://en.wikipedia.org/wiki/Common_reference_string_model) in cryptography). You can think of the common reference string as some fundamental property of the universe, or in practice a deterministic process that's effectively impossible to predict because it is so complicated. To be honest, this is a fairly large hole in my argument. On the other hand, it seems fairly tenuous to anchor the necessity of a computer within simulationism on the necessity of a universe from which to derive randomness.

[^5]: This would be a possible, though obviously hacky, modification of the laws of our own universe
