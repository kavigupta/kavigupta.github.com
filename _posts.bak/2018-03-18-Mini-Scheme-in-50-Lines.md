---
layout: post
title: (Mini) Scheme in 50 Lines
comments: True
---


The scheme language is a programming language that is unique for being easy to parse. Every construct in scheme is of the form

```
(keyword arg1 ... argn)
```

## Our Subset

We will start with some standard scheme expressions, which you can think about as being analogous to certain patterns in Python. For example instead of writing `a + b`, you write `(+ a b)`, where `+` is just the name of a function. Similarly, instead of writing `lambda x: x * 2` for the doubling function, you write `(lambda (x) (* 2 x))`. Just lambdas and function application are enough to perform any calculation, but we add a few more features for clarity: instead of `x = y` we write `(define x y)`, instead of `x if y else z` we write `(if y x z)` and instead of writing

```python
x = f(y)
return x
```

we write `(begin (define x (f y)) x)`. The real scheme language has many more constructs, including ones that simulate python's `def` statements, and some unique ones that allow you to assign variables in a local frame, simulate `elif` trees, have short-circuited `and/or` constructs, or even define your own language constructs. For brevity, we will stick to this subset, which is still very powerful.

<!-- end excerpt -->

## Lexing

The first step in processing a scheme program is to split it up into a list of tokens. What we do is take an expression like `'(a b (c))'` and convert it into a list of tokens like `'(', 'a', 'b', '(', 'c', ')', ')'`.

To accomplish this, we first pad all the parentheses with space and replace all space-like characters with a space. We then split on spaces (and filter out empty tokens, which are produced when multiple spaces are consecutive).

```python
def lex(text):
    text = re.sub("([()])", r" \1 ", re.sub(r"\s", " ", text))
    return [x for x in text.split(" ") if x]
```

## Parsing

We now need to parse the string. If we have `'(a b (c))'`, we want to parse it to `['a', 'b', ['c']]` so we can process it in Python. Parsing scheme is fairly simple using recursion. Whenever we see a symbol or number, we just return and move on. Otherwise, we recursively call parse until we see an end parenthesis. For example, if we have the current state of our input

```
(+ 2 (4) (* 5 3)) 2 3)
```

we parse by removing the `(` from the front, then parse `+`, `2`, `(4)`, and `(* 5 3)` before seeing the unmatched `)`. Then we return, having removed everything but the `2 3)`. This is implemented as follows:

```python
def parse(text):
    text = lex(text)[::-1]
    def parse():
        start = text.pop()
        if start != '(':
            return start
        vals = []
        while text[-1] != ')':
            vals += [parse()]
        text.pop()
        return vals
    return parse()
```

The reason we reverse the text is so that `pop()` pops from the end rather than the front of the input stream.

## Frames

We need to define an environment diagram so that we can execute code. This is similar to a Python style environment diagram, which is a backwards pointing tree (children point towards the root via the parent annotation rather than the other way around).

To define a frame, we use the `defaultdict` class from python, which is like a dictionary except that when it can't find a key, instead of raising a `KeyError`, it calls a 0 argument function you provide in the constructor and sets that as its value. We want to slightly modify this so that it calls a 1 argument function. Thus we define a frame as such:

```python
class frame(defaultdict):
    def __init__(self, f):
        super().__init__(lambda: None)
        self.__function = f
    def __missing__(self, key):
        return self.__function(key)
```

`defaultdict` calls the `__missing__` function when necessary, and in this case, it calls the parent function on a key. We can create a child frame for a given frame simply by saying `frame(lambda v: parent[v])`. We can now create a global frame as such:

```python
global_frame = frame(int)
global_frame.update({"+" : add, "-" : sub, "*" : mul, "/" : floordiv, "=" : lambda x, y: x == y})
```

We do something a little hacky here by basically defining integers as just being variables that evaluate to the integer version of themselves via the function `int`. (this works as `int("123") == 123`).

## Special Forms

Now we need to somehow handle our special keywords (define, lambda, begin, if).

### Define

We assume that we already have a function `seval` defined, which is the function that will eval a scheme expression. In scheme, `define` returns the variable being assigned to, for reasons of tradition.

```python
def define(exp, env):
    env[exp[0]] = seval(exp[1])
    return exp[0]
```

### Lambda

A lambda expression has no side effects, but must return a function. First we create a new frame, then we assign the operator to the operands, and then we run the body of the function in that frame and return the last value:

```python
def slambda(exp, env):
    def do_lambda(*args):
        local_env = frame(lambda x: env[x])
        local_env.update(dict(zip(exp[0], args)))
        return [seval(u, local_env) for u in exp[1:]][-1]
    return do_lambda
```

### Begin

This is similar to `lambda` except that we don't have any arguments or a new frame, and this one is simple enough to just write as a Python lambda function.

```python
lambda exp, env: [seval(u, env) for u in exp][-1]
```

### If

We can just directly map this to the equivalent Python construct.

```python
lambda exp, env: seval(exp[1], env) if seval(exp[0], env) else seval(exp[2], env)
```

### Putting it together

We create a dictionary of special forms for easy access:

```python
def define(exp, env):
    env[exp[0]] = seval(exp[1])
    return exp[0]
def slambda(exp, env):
    def do_lambda(*args):
        local_env = frame(lambda x: env[x])
        local_env.update(dict(zip(exp[0], args)))
        return [seval(u, local_env) for u in exp[1:]][-1]
    return do_lambda
special_forms = {
    "define" : define,
    "lambda" : slambda,
    "begin" : lambda exp, env: [seval(u, env) for u in exp][-1],
    "if" : lambda exp, env: seval(exp[1], env) if seval(exp[0], env) else seval(exp[2], env)
}
```

## Eval Function

To evaluate a parsed scheme tree, what we need to do is dependent on whether the input is a list or not. If we have a list, then we need to check if its first element is a special form, and if so run its special form function. Otherwise, we evaluate the first item as a function, then evaluate the rest of the items as its arguments, then call the function. If we don't have a list, we just look up the current element in the current frame.

```python
def seval(tree, env=global_frame):
    if isinstance(tree, list):
        func, *args = tree
        if func in special_forms:
            return special_forms[func](args, env)
        return seval(func, env)(*(seval(x, env) for x in args))
    return env[tree]
```

We can then run scheme by running `seval(parse(text))`.

## The Entire Interpreter

The entire interpreter, which is just 50 lines, is as below:

```python
from collections import defaultdict
from operator import *
import re

def lex(text):
    text = re.sub("([()])", r" \1 ", re.sub(r"\s", " ", text))
    return [x for x in text.split(" ") if x]
def parse(text):
    text = lex(text)[::-1]
    def parse():
        start = text.pop()
        if start != '(':
            return start
        vals = []
        while text[-1] != ')':
            vals += [parse()]
        text.pop()
        return vals
    return parse()
class frame(defaultdict):
    def __init__(self, f):
        super().__init__(lambda: None)
        self.__function = f
    def __missing__(self, key):
        return self.__function(key)
global_frame = frame(int)
global_frame.update({"+" : add, "-" : sub, "*" : mul, "/" : floordiv, "=" : lambda x, y: x == y})

def define(exp, env):
    env[exp[0]] = seval(exp[1])
    return exp[0]
def slambda(exp, env):
    def do_lambda(*args):
        local_env = frame(lambda x: env[x])
        local_env.update(dict(zip(exp[0], args)))
        return [seval(u, local_env) for u in exp[1:]][-1]
    return do_lambda
special_forms = {
    "define" : define,
    "lambda" : slambda,
    "begin" : lambda exp, env: [seval(u, env) for u in exp][-1],
    "if" : lambda exp, env: seval(exp[1], env) if seval(exp[0], env) else seval(exp[2], env)
}
def seval(tree, env=global_frame):
    if isinstance(tree, list):
        func, *args = tree
        if func in special_forms:
            return special_forms[func](args, env)
        return seval(func, env)(*(seval(x, env) for x in args))
    return env[tree]
```

Of course, a proper scheme interpreter, which has more features (see the section "Subset"), more possible types (like strings, symbols, lists, etc.), and better error handling (we crash on a lot of cases with `IndexError`s and `ValueError`s). In fact, that's an entire project in 61a.

But I personally enjoy that you can get quite a few features in such a small amount of space.

Try it out on the following program!

```
(begin
    (define factorial
        (lambda (x) (if (= 0 x) 1 (* x (factorial (- x 1))))))
    (factorial 40))
```
