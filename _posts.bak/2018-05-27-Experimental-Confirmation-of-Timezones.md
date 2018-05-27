---
layout: post
title: Confirming that timezones exist with minimal trust
comments: True
---

I was listening to an episode of [Oh No, Ross and Carrie!](https://en.wikipedia.org/wiki/Oh_No,_Ross_And_Carrie!) about Flat Earth and it struck me that if you believe the world is conspiring against you, it's practically quite difficult to prove almost anything. Now one thing that most Flat Earthers do seem to accept is that when its day in one part of the planet, it's night in another part.

However, let's say someone denied this. How would you prove it, with the minimum amount of trust?

## Can we do this while trusting nobody?

Let's say that our experimenter only trusts their own senses. Can they prove that the day/night dichotomy exists? Well, one way they could do this would be to confirm that their watch is correct, then fly to the other side of the world, then confirm that their watch was off by 12 hours from the day/night cycle. However, this isn't exactly right, it's possible that someone could mess with your watch to cause it to run slightly fast or slow and cause it to be different when you alived as when you left.

With a fast enough plane, you might be able to confirm that day had turned to night in less than 12 hours, without the need to rely on a possibly manipulable watch. However, the very act of stepping on a plane might allow nefarious agents to manipulate your subjective experience of time, thus fooling you into thinking time zones exist. (Yes, this is a massively unproductive view of the world, but let's try to work around it.)

Given these limitations, it's clear that our prover needs a collaborator they can trust.

## Basic protocol with a trusted collaborator

We now have two experimenters, who each trust each other and want to prove the existence of time zones: Alice and Bob. Consider the following protocol: Bob travels across the world while Alice stays behind. Alice and Bob then communicate with each other and confirm that one sees day and the other night.

This works right? Well, almost. The issue is that there's no protection against a simulation attack: what if Alice speaks to a simulated version of Bob while Bob speaks to a simulated version of Alice, with the two conversations actually taking place 12 hours apart from each other?

Alice and Bob's position is starting to look undisprovable. But is it?

## Shared Password

Let's say that before leaving, Alice and Bob come up with a shared password, \\(p\\), randomly. Then, the protocol is as such:

 - Alice and Bob: agree on password \\(p\\)
 - Alice: goes to other side of the world and confirms that its day
 - Alice: sends \\(p\\) to Bob
 - Bob: confirms that its night and that the received message is in fact \\(p\\)

But wait, the adversary could trap the password after Alice has sent it but before Bob has received it, thus delaying it 12 hours and allowing the protocol to go through, even if the world in fact has no time zones. The issue is that there's no response from Bob. Bob just sending back the password doesn't help, since the adversary could do that!

## Different Passwords

OK, so now we have two different passwords \\(p_a\\) and \\(p_b\\). The protocol is now as follows:

 - Alice and Bob: create passwords \\(p_a\\), \\(p_b\\) and share them with each other
 - Alice: goes to other side of world and confirms that its day
 - Alice: sends \\(p_a\\) to Bob
 - Bob: confirms that its night and that the received message is \\(p_a\\)
 - Bob: sends \\(p_b\\) to Alice
 - Alice: confirms that it is still the same day and the received message is \\(p_a\\)

Now, Alice knows for sure that the world in fact has multiple time zones.

However, Alice and Bob might say that the adversary has the ability to read and edit their memories and thus steal their passwords. OK, this objection must make their claim undisprovable, right? In this case the answer is yes, because the adversary could always just convince them that the world had timezones by making arbitrary modifications to their memories.

However, if we give the adversary only read access to their memories, their position turns out not to be undisprovable!

## Secure Time Synchronization

The issue that Alice and Bob are running into is that they can't pre-agree on the passwords, because then they know the passwords ahead of time and thus the adversary knows the passwords ahead of time. However, they might be able to "post-agree" on the passwords, as such:

 - Alice and Bob: start on opposite sides of the world
 - Alice: Confirms that its day
 - Alice: Generates and sends \\(p_a\\) to Bob
 - Bob: Confirms that its night
 - Bob: Generates and sends \\(p_b\\) to Alice
 - Alice: Confirms that it is still the same day
 - Alice: meets with Bob
 - Alice and Bob: check that all passwords transmitted were correct

If this protocol goes through without Alice and Bob having any issues, then they know that it can be day and night at the same time in different parts of the world. The procedure works!

## Assumptions and Proof

OK, so if we wanted to prove that the above scheme works, we would need a few assumptions. First, we assume Alice and Bob are trustworthy. Second, we need to assume that Alice can confirm that some entity is Bob in a face-to-face conversation and vice versa (so that the Alice and Bob at the end are guaranteed to be the real Alice and Bob). Third, we need to assume that Alice and Bob can tell when night falls, that is, the adversary can't somehow convince them that two points in time are on the same day when it was in fact night in between those points. Finally, the adversary cannot have time travel and cannot predict either Alice or Bob's random number generators.

In this case, we can prove that Bob confirming that it is night where he is happened after Alice generated \\(p_a\\) and thus during some day D where Alice was, and before Bob generated and thus before Alice received \\(p_b\\) and thus before some point on the same day D. Since we assume days are continuous, this proves that Bob observed night during a time when Alice observed day, and thus this procedure is secure.

You can weaken the assumption that two specific trustworthy people are needed by repeating this procedure for every pair of people in some set of N people. Then the scheme proves that time zones exist assuming that 2/N of the people are trustworthy, at the cost of having to repeat the scheme \\(O(N^2)\\) times

## Implications

N/A.

Less facetiously, I guess my general takeaway is that "your claim is undisprovable" shouldn't be used as a synonym for "you're making ridiculous, conspiratorial, assumptions". Because even when someone is being paranoid, there might be a way to disprove their hypothesis, with enough effort.
