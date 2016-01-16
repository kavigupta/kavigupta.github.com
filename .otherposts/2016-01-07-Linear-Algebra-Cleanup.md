---
layout: post
title: Linear Algebra Image merge
comments: True
---

The other day my cousin and I wanted to take pictures of the sky, but the camera had a maximum shutter period of 30 seconds. Therefore, we set it up to take 10 consecutive 30 second pictures, which I promised to stitch together later.

## [And Yet It Moves](https://en.wikipedia.org/wiki/And_yet_it_moves)

Of course, if the earth was stationary, this would be pretty simple; just take the algebraic sum of the images and project onto some reasonable range. However, the earth is moving (OK, in this case, the major problem is rotation, not what Galileo was talking about), which means we need to take into account the apparent rotation of the images.

These insets demonstrate that a simple read is not effective; first is a simple crop of a cluster of stars, the other is an overlay of all the images. As you can see, the composite is highly blurred.

<img src="resources/2016-01-07/gen/original-cropped.png">
<img src="resources/2016-01-07/gen/original-avgd.png">

## Matrix Representation

You can probably tell that the blur is roughly linear, but we don't particularly want to count on this fact. In general, the stars will appear to move in a circular pattern, rotating as well as translating.

Fortunately, there is a simple way to represent this. We represent each pixel as a vector \\(\langle x, y, 1 \rangle\\) and each transformation as a 3x3 matrix which preserves the 1 in the last column. Matrices of this form must satisfy the equation

\\[gx + hy + j = 1\\]

for all \\(x, y\\) where \\((g, h, j)\\) are the final row of the transformation. Since this must work for all \\(x, y\\), the only possible values are \\(g = 0, h = 0, j = 1\\). Since the last row is determined, we have effectively a 6-parameter function:

\\[\mathrm{transform} : \mathbb{R}^6 \rightarrow (\mathbb{Z}^2 \rightarrow \mathbb{Z}^2)\\]

This function is specified (where \\(a\\) - \\(f\\) are the first two rows of the matrix) as

\\[(x_f, y_f, 1) = (ax + by + c, dx + ey + f, 1)\\]

which, given fixed values for \\((x, y)\\) is effectively two independent linear functions.

## Star matching

To be able to find the coefficients \\(a,b,c,d,e,f\\), we need a large number of data points \\((x, y), (x_f, y_f)\\). To do this, we will match star centers. This is accomplished by the method of star identification. This is accomplished by [StarIdentification.java](/resources/2016-01-07/src/StarIdentification.java).

First, clusters of pixels representing stars need to be found. This was done by a cluster-finding algorithm. The results, applied to the same segment, were as follows (with random stars being assigned random colors):

<img src="/resources/2016-01-07/gen/clusters.png">

And here are two cluster graphs layered on top on top of each other

<img src="/resources/2016-01-07/gen/clusters-two.png">

Then, the brightest star was measured and stars of similar brightness were located around it. The offsets necessary to find these stars (we are assuming that just offsetting everything is good enough for now to get a good guess) were then tabulated and compared using other stars to find the correct match. Here is an inset of the matches for the entire star field applied to this section:

<img src="/resources/2016-01-07/gen/matchedStars.png">

## Overlaying the images

Overlaying the images ends up being fairly simple. Basically, a matrix is created of the same size as the original image, and then each image is transformed with RGB values being placed in neighboring boxes according to the last few decimal places of the image.

To accomplish this, each box must contain four columns: `(red, green, blue, weight)`, which must all be doubles. Then, each of the RGB is divided by the weight. For a proper composite image a reasonable mapping should be found back into the RGB colorspace. This can be achieved by using a 
