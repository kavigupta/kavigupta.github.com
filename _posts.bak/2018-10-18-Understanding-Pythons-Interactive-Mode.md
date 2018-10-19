---
layout: post
title: Understanding Python's interactive mode
comments: True
---

The Python interepreter has this convenient feature that when you type something simple onto the interpreter, it spits that value back out at you, so you can copy/paste it back as a value.

But with convenience often comes complexity, let's look at how this feature works!

<!-- end excerpt -->

## How does the print function work?

Before we can talk about typing stuff onto the interpreter, let's talk about the simpler function `print`. Let's take a look at a few examples:

```
>>> print('abcdef')
abcdef
>>> print('1')
1
>>> print(1)
1
>>> x = print(1)
1
>>> x
>>>
```

So how does `print` work? Clearly, it returns `None` once it's done. But what's it doing when it's printing to the screen. Clearly, something strange is happening where both the string `'1'` and the number `1` get printed out as the single character `1`.

To properly understand print, we need to break it down into its constituent parts. Imagine we have a function `print_string`, which takes a string and prints it to a terminal. Then, we have the following behavior:

```
>>> print_string('abcdef')
abcdef
>>> print_string('1')
1
>>> print_string(1)
Error
```

Which is somewhat more consistent. We can implement `print_string` in terms of print fairly easily as follows:

```python
def print(x):
    print_string(str(x))
```

where `str` is a function that takes in a value and outputs some string. We have the following behavior for `str` (illustrated using lists for reasons that will become clear later).

<iframe width="800" height="250" frameborder="0" zoom="0.4" src="httpss://pythontutor.com/iframe-embed.html#code=x%20%3D%20list%28str%2812%29%29%0Ay%20%3D%20list%28str%28'12'%29%29&codeDivHeight=400&codeDivWidth=350&cumulative=true&curInstr=2&heapPrimitives=nevernest&origin=opt-frontend.js&py=3&rawInputLstJSON=%5B%5D&textReferences=false"> </iframe>

In the case of a string, it does nothing, and in the case of an integer, it converts it into a string. We'll come back to `str` later, but for now think of it as providing a nice human-readable version of the given object.


## `repr`

But if we want an output that isn't human-readable, but is instead computer-readable, we can use `repr` instead. For example, `repr(1)` and `repr('1')` should produce different outputs as they should be read as two different types. Let's look at some examples:

```
>>> print_string(repr('abcdef'))
'abcdef'
>>> print_string(repr('1'))
'1'
>>> print_string(repr(1))
1
```

> Note for later: `print_string(repr(<blah>))` prints out `<blah>`

Looking at an the same example as before using an environment diagram, we have the same result for `repr(12)` as `str(12)` but `repr('12')` has a set of quotes around it while `str('12')` does not.

<iframe width="800" height="250" frameborder="0" src="https://pythontutor.com/iframe-embed.html#code=x%20%3D%20list%28repr%2812%29%29%0Ay%20%3D%20list%28repr%28'12'%29%29&codeDivHeight=400&codeDivWidth=350&cumulative=true&curInstr=2&heapPrimitives=nevernest&origin=opt-frontend.js&py=3&rawInputLstJSON=%5B%5D&textReferences=false"> </iframe>

This difference can be attributed to the fact that typing in `12` gets us 12, while we need to type in `'12'` to get `'12'`, whereas `12` appropriately represents both concepts to a human reader.

## Typing something into the interpreter

In the last section, we saw how the `repr` function works, especially on strings, where it adds a layer of quotes. You might notice that typing something directly onto the interpreter performs a similar task. That's because it more or less is. We can consider typing something directly into the interpreter (`>>> x`) to be the same as calling `>>> print_string(repr(x))`. The examples from above have the same behavior:

```
>>> 'abcdef'
'abcdef'
>>> '1'
'1'
>>> 1
1
```

In fact, this is the motivation behind `repr`, if you type something into the interpreter, it spits it back out at you.

## Typing `repr(something)` into the interpeter

Let's say you did the following:

```
>>> repr('hi')
"'hi'"
```

What happened? We now have two sets of quotes around the `hi`. Did the `repr` function add two sets of quotes instead of one? The answer is no, because, as you can recall from the previous section, that example is equivalent to

```
>>> print_string(repr(repr('hi')))
"'hi'"
```

Using our environment diagram, we can see the following:

<iframe width="800" height="250" frameborder="0" src="https://pythontutor.com/iframe-embed.html#code=x%20%3D%20list%28repr%28'hi'%29%29%0Ay%20%3D%20list%28repr%28repr%28'hi'%29%29%29&codeDivHeight=400&codeDivWidth=350&cumulative=true&curInstr=2&heapPrimitives=nevernest&origin=opt-frontend.js&py=3&rawInputLstJSON=%5B%5D&textReferences=false"> </iframe>

We see that `repr('hi')` has four characters in it: `'`, `h`, `i`, and `'`. `repr(repr('hi'))` needs to represent this, so it has six characters in it: `"`, `'`, `h`, `i`, `'`, `"`. These six characters get printed to the terminal, to form the string `"'hi'"`.


## `__str__` and `__repr__`

OK, so now that we know that `str`, `repr`, `print`, and typing something directly into the terminal do, let's look at how these concepts apply to user-defined functions. Take the following class:

```python
class A:
    def __repr__(self):
        return "hi"
    def __str__(self):
        return "bye!"
```

The idea is that whenever we call `str(x)` when `x` is of type `A`, this is equivalent to calling `x.__str__()`, and whenever we call `repr(x)` when `x` is of type `A`, this is equivalent to calling `x.__repr__()`.

## Examples

Here are some examples of how custom `__str__` and `__repr__` methods work in practice. Before we get into the explanations, try to figure them out on your own

```
>>> a = A()
>>> a
hi
>>> print(a)
bye!
>>> repr(a)
'hi'
>>> str(a)
'bye!'
```

Your first question might be: why no parentheses when we type `a` into the terminal? The answer is that `>>> a` is equivalent to `print_string(repr(a))`, and we have that `repr(a)` is the string of length 2 containing the letters `h`, and `i`; which then gets printed. On the other hand, `>>> repr(a)` is equivalent to `print_string(repr(repr(a)))`, and `repr(repr(a))` is the string of length 4 containing the letters `'`, `h`, `i`, and `'`. The following environment diagram describes how this process happens:

<iframe width="800" height="500" frameborder="0" src="https://pythontutor.com/iframe-embed.html#code=class%20A%3A%0A%20%20%20%20def%20__repr__%28self%29%3A%0A%20%20%20%20%20%20%20%20return%20%22hi%22%0A%20%20%20%20def%20__str__%28self%29%3A%0A%20%20%20%20%20%20%20%20return%20%22bye!%22%0A%0Aa%20%3D%20A%28%29%0Afirst_line%20%3D%20list%28repr%28a%29%29%0Asecond_line%20%3D%20list%28str%28a%29%29%0Athird_line%20%3D%20list%28repr%28repr%28a%29%29%29%0Afourth_line%20%3D%20list%28repr%28str%28a%29%29%29%0Adel%20a,%20A%20%23%20remove%20extra%20stuff%20from%20environment%20diagram&codeDivHeight=400&codeDivWidth=350&cumulative=false&curInstr=13&heapPrimitives=nevernest&origin=opt-frontend.js&py=3&rawInputLstJSON=%5B%5D&textReferences=false"> </iframe>
