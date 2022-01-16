preprocess:
    replace "IMAGE: ([^;\n]+);?(.*)" -> "<center><img src=\"/resources/2018-05-25/\\1.png\" \\2 /></center>"
    replace "<!--a*-->" -> ""
---
layout: post
title: Absence of Evidence is Evidence of Negligibility
comments: True
---

A saying of Martin Rees, which Sagan popularized, is "Absence of evidence is not evidence of absence." However, this doesn't sit exactly right with me.

## Evidence of absence is an impossible bar to meet

This is one of the central concepts of skepticism: positivism. There is no way to prove that there is no effect, because the effect could always be smaller than you believe it is. For example, take this (computer-generated) data:

IMAGE: drug_vs_placebo

You might say that it looks like the two of those have the same effect, of about 2 units, and conclude that the drug has no effect. The statistics would back you up here: a bootstrap difference of means test gives a two-sided p-value of 15%, which is greater than 5%, so it is not considered a significant difference.

However, lets say we redo this study with

[^1]: Yeah, I had no idea this wasn't Sagan's either, apparently its a [misattribution](https://en.wikiquote.org/wiki/Carl_Sagan).
