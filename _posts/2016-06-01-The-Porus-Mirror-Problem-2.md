---
layout: post
title: The Porus Mirror Problem, Part 2
comments: True
---

## Propagation

From last time, we know the quantity of light that escaped at each location and direction around the unit circle. We want to find how this translates into quantities of light at any point away from the circle. This situation can be modeled as so:

<img src="/resources/2016-06-01/propagation.png" />

<!--end excerpt-->

The basic idea is to find for any given \\(\alpha, r\\) the set of possible \\(\phi, \theta\\) values. In any case, it is easiest to eliminate the \\(\phi\\) variable via rotation.

<img src="/resources/2016-06-01/propagation-phi.png" />

Noting that the two angles have the same \\(y\\) coordinate, we end up with the equation:

\\[r\sin(\alpha-\phi) = \sin(\theta-\phi)\\]

Therefore, we can calculate \\(\theta\\) in terms of everything else as

\\[\theta = \phi + \sin^{-1} \left(r\sin(\alpha-\phi)\right)\\]

This is defined whenever \\( -1 \leq r\sin(\alpha-\phi) \leq 1 \\), which is true whenever \\(\alpha-\sin^{-1} \frac{1}{r} \leq \phi \leq \alpha + \sin^{-1} \frac{1}{r}\\).

We can therefore calculate the quantity of light at any point as

\\[E (r, \alpha) = \int_{\alpha-\sin^{-1} \frac{1}{r}}^{\alpha+\sin^{-1} \frac{1}{r}}L\left(\phi, \phi + \sin^{-1} \left(r\sin(\alpha-\phi)\right)\right)\text{d}\phi\\]

## A constant transparency

As might be expected, a constant transparency yields a graph of \\(L\\) perfectly symmetrical across \\(\phi = \theta\\): it is only a function of \\(\phi - \theta\\). (The following graph has \\(\theta\\) on the \\(x\\) axis and \\(\phi\\) on the \\(y\\).)

<img src="/resources/2016-06-01/img/const-phitheta.png" />

This yields the symmetrical circular pattern of energy

<img src="/resources/2016-06-01/img/const-pattern.png" />

## Shifted \\(\sin\\)

To get a less symmetrical pattern, we can shift the \\(\sin\\) function into \\([0,1]\\). This function is effectively \\(y\\) so results in the top-biased:

<img src="/resources/2016-06-01/img/sin_shift-phitheta.png" />

<img src="/resources/2016-06-01/img/sin_shift-pattern.png" />

## Periodic pattern on surface

Using the 3-period periodic function \\(\sin^2 \frac{3\theta}{2}\\), we get a 3-petal flower:

<img src="/resources/2016-06-01/img/sin_3-phitheta.png" />

<img src="/resources/2016-06-01/img/sin_3-pattern.png" />

## Half-opaque

Making the bottom half of the circle opaque leads to the diagram:

<img src="/resources/2016-06-01/img/half_opaque-phitheta.png" />

<img src="/resources/2016-06-01/img/half_opaque-pattern.png" />

## A Tiny Opaque piece

A small opaque piece casts a small shadow

<img src="/resources/2016-06-01/img/small_refl-phitheta.png" />

<img src="/resources/2016-06-01/img/small_refl-pattern.png" />

## A Single Slit

Because our model is corpuscular, we don't have diffraction.

<img src="/resources/2016-06-01/img/small_slit-phitheta.png" />

<img src="/resources/2016-06-01/img/small_slit-pattern.png" />

## A Double Slit

Again, no diffraction. The pattern that occurs is mostly an aliasing effect.

<img src="/resources/2016-06-01/img/double_slit-phitheta.png" />

<img src="/resources/2016-06-01/img/double_slit-pattern.png" />

## Negative Slit

Our model is flexible enough to allow for "negative transparency", when "negative light" is projected forwards while extra light is reflected backwards. This is basically just a color inverted version of a small reflector.

<img src="/resources/2016-06-01/img/negative_slit-phitheta.png" />

<img src="/resources/2016-06-01/img/negative_slit-pattern.png" />

