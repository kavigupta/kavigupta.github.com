preprocess:
    include
---
layout: post
title: Setting the actual size of figures in matplotlib.pyplot
comments: True
---

Let's say you want to set the size of a figure in matplotlib, say because you want the captions to match the font size on a poster (this came up for me recently). However, you might find yourself with kinda a  weird problem.

# Directly setting the size of a figure

Setting the actual size of figures in matplotlib.pyplot is *difficult*. Setting the size of a figure as so

```python
include "../_resources/2019-05-18/direct.py"
```

<!-- end excerpt -->

<img src="/resources/2019-05-18/direct.png" border="2"/>

It's the right size in pixels, except that this image is padded weirdly and the x label is cut off (border added).

# Tight Layout

The fix for this is to use a tight layout in the output

```python
include "../_resources/2019-05-18/tight-layout.py"
```

<img src="/resources/2019-05-18/tight-layout.png" border="2"/>

This fixes the elements being dropped issue, but the image size is now incorrect (at 2.59x4.62 instead of 2.5x5).

# Fixing the problem

First, we write a general function to get the size of a figure. We then calculate \\(x_{\\text{to set}} = x_{\\text{set previously}} \\frac{x_{\\text{target}}}{x_{\\text{actual}}}\\) for \\(x\\) being the width and height. The intuition behind this equation is that we figure out how off the actual image's size is from our target, and use this to update what we tell matplotlib to do.


However, this is an approximation, and we repeat it to get a better fit. The full code is below:

```python
include "../_resources/2019-05-18/set_fig_size.py"
```

Using this method (note that we no longer need to set the figure size when creating the figure)

```python
include "../_resources/2019-05-18/update-size.py"
```

we get the following result, which is exactly the correct size.

<img src="/resources/2019-05-18/update-size.png" border="2"/>

# What's with the return False?

The reason that we check to see if the error is increasing is that sometimes it is impossible to fit a plot within a given space. For example, take the following example

```python
include "../_resources/2019-05-18/update-size.py"
```

This ends up trying to decrease the height of the figure to below 0, but before it can cause a crash in `matplotlib`, it causes the last condition in `set_size` to be activated, leading to a False return value. The resulting image is 91x109, nowhere close to the 100x50 that it should be, and looks like this

<img src="/resources/2019-05-18/bad.png" border="2"/>


By returning `False`, this allows e.g., a notebook to finish executing in cases where it is unecessary to check, but a simple `assert set_size(fig, (1, 0.5))` would lead to an error if that is desired.

# Conclusion

The example above is full, working, code, and should honestly probably be part of `matplotlib`: generally speaking, when I set the size of a figure, I intend to set the actual size of the figure, not the size of the plot with some minimal padding around it. In any case, hopefully this code is helpful to you!
