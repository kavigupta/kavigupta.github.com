---
layout: post
title: Visualizing a Force Field
comments: True
---

A force field is a space in which for a small object (whose impact on the field is negligible) there is a force acting on it. Examples include gravity and electromagnetism.

This post is about visualizing such a space without resorting to unsatisfying arrow diagrams or contour maps.

## Simplifying a force field

Let's simplify a little. Do we really need the mass part of the force field? To clarify, we know that

\\[\mathbf F = m \mathbf a\\]

And our particles have fixed mass, so we can describe our field as an acceleration field.

## The situation

Imagine a continuum of particles of negligible in the field. We will solve for a situation in which the field is unchanging with time, i.e., whenever a particle moves it is replaced by another particle with the same velocity. Since this is a relatively continuous situation, we will consider the quantity \\(q\\) of particles at a given point and given velocity to be some real number.

## The equations

We can define a state of a particle as \\(\mathbf S = (x, y, v_x, v_y)\\), where all quantities in the equation are parameterized in time. The quantity function is a function of this state. Because of our invariance condition, we have:

\\[\newcommand{\di}{\text{d}} q(\mathbf S(t + \di{t})) = q(\mathbf S(t))\\]

We know that

\\[(x, y, v_x, v_y)(t + \di{t}) = (x + v_x\di{t}, y + v_y\di{t}, v_x + a_x\di{t}, v_y + a_y\di{t})\\]

So therefore we have

\\[q(x, y, v_x, v_y) = q(x + v_x\di{t}, y + v_y\di{t}, v_x + a_x\di{t}, v_y + a_y\di{t})\\]

## The numerical solutions

We can consider our solution to be a vertical line sweeping across our 2D space and determining \\(q\\) along that line. In general, we need to calculate \\(q(x, y, v_x, v_y)\\) in terms of \\(q(x-1, ?, ?, ?)\\) where the question marks are filled in by unknown quantities. We can them sum \\(q\\) over all \\(v\\) to get the total quantity at that point.

We can calculate \\(\di{t}\\) by setting \\(v_x\di{t} = -1\\) and substitute, yielding the equation:

\\[q(x, y, v_x, v_y) = q(x - 1, y - \frac{v_y}{v_x}, v_x - \frac{a_x}{v_x}, v_y - \frac{a_y}{v_x})\\]

##