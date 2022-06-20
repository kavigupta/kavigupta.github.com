preprocess:
    include
---
layout: post
title: Sampling: a different kind of election map
comments: True
tags: draft
---

Election maps tend to obscure a couple things that I think are interesting about American
elections.

First, about 33% of Trump supporters, and about 30% of Biden supporters lived in
precincts that voted for the other party, so show up in the other party's color on the map!

Second, the map tends to overstate the number of people who live in purely rural areas.

The combination of these two effects leads to in my view a fairly big distortion of our pictures
of blue and red America, but especially red America, which people begin to picture as entirely
middle-of-nowhere rural areas.

Instead, here's a sampling of 100 voters and what their neighborhoods look like. This has the
benefit of giving you a picture of what
Biden's America and Trump's America look like: not two disjoint nations, but two overlapping
and intertwined distributions. You can click each dot to jump down to the voter represented
by the dot.

<iframe id="igraph" scrolling="no" style="border:none;" seamless="seamless" src="/resources/2022-06-20/graph.html" height="525" width="100%"></iframe>

Biden's voters are placed on the left, while Trump's are on the right. I have provided
an image of what their street might look like on Google Streetview, as well as the lean of
their precinct. If you click on the image, it will take you to a Google maps link for their
precinct's center, so you can see where it is.

I think, beyond everything else, one of the biggest takeaways I have is just how empty suburban
areas in the South and Midwest are. I grew up in a fairly dense suburb of LA, where lot sizes are
about a sixth of an acre and have lived in small cities ever since, so I pictured, e.g., "Georgia suburbs"
to be similar. Instead, they look a lot like what I picture when someone says "rural America".

*Technical details: to sample the voters I first figured out how many Trump and
how many Biden voters should be picked, then for each candidate I sampled precincts
randomly, weighted by how many of that candidate's voters lived in the precinct.
I excluded Kentucky from this sampling as I do not have precinct data for Kentucky.
The precinct data is from a combination of the NYT and VEST and I try to find a street with
a "street-like" address (so not a freeway or major road) inside the precinct,
whenever possible. The images should be taken as a broadstrokes vibe of the precinct
rather than something super specific.*

include "../resources/2022-06-20/index.html"