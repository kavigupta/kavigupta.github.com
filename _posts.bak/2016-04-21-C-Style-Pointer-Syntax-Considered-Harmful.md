---
layout: post
title: C-Style Pointer Syntax Considered Harmful
comments: True
---

## Declaration Follows Use

In C, a pointer to int has type `int*`, but really, this isn't the right way of looking at it. For example, the following declaration:

```C
int* x, y;
```

creates `x` of type `int*` and `y` of type `int`.

In reality, the correct formatting is

```C
int *x, y;
```

Which is read not "x is a pointer to int" but rather `*x` and `y` are ints. Similarly, when creating an array of 20 ints named `x` we say:

```C
int x[20];
```

Which is read `x[i]` is an int for `0 <= i < 20`. The egregious example of DFU is function pointer declarations. For example, a declaration to a function pointer of a function from int to int is:

```C
int (*f)(int);
```

which is read that `(*f)(int)` is of type `int`, meaning that `*f` is of type `int -> int`.

## Complex Declarations

The basic problem with DFU is that it doesn't scale. The website [cdecl.org](http://cdecl.org/) demonstrates some of the most egregious examples of this ridiculousness.

It cites, for example,

```C
char (*(*x())[5])()
```

as "declare x as function returning pointer to array 5 of pointer to function returning char"

These declarations require a sort of backwards thinking to understand. You need to work your way out from the inside: the first thing is the `x()`, which says that `x` is a function, which can be dereferenced (the `*` to the left of the `x`), and so on, and so on.

## Inconsistencies

So, C-Style pointer declarations are a bit annoying to use, but that is just the way they are, right? You just have to think the way C does and then everything starts working out, right?

Wrong.

For example, `int *x;` declares `*x` as an int, and `*x = 2` assigns `*x` to 2, so `int *x = 2` should declare `*x` as an int and assign it to `2`?

Actually, no. `int *x = 2` actually assigns `x` to `2`, which hopefully will be a compiler error, but for those who ignore warnings, will silently set `*x` to an integer at the third byte of memory. On certain systems, this might not segfault, but rather lead to overwriting some important part of memory, hopefully leading to a crash rather than just weird behavior.

## Declaration Follows Initialization

The general idea of DFI (which is a name I think I may have invented, given Google's insistence that the three words don't ever appear in that order) is that a type should be an atomic unit, separate from the function name. Additionally, it should follow how to get a value of that type.

A lot of the original C syntax seems to exist to allow for simpler parsing since types and variables have to coexist on the same side of the `=`, therefore I think that Pascal style postfix types are preferable. I will use `#` to allow for an easier differentiation from other symbols in C.

This will probably make more sense with examples:

## Pointers

How do you get a pointer? By taking the address of an lvalue, of course! So, if `x` is the address of an int, it should have type `&int`. An example is as follows:

```C
x # int;
x = 2;
y # &int;
y = &x;
```

## Arrays

How do you get an array? By array initialization notation! So, if `x` is an array of int, it should have type `{int}`. To encode a length, we can use the format `{int, 20}`. To declare the array `{1,2,3,4,5}`, we can use the following code block:

```C
x # {int, 5};
x = {1,2,3,4,5};
```

## Functions

OK, so we can't actually create function expressions in C (and this syntax change isn't really the right place to introduce lambdas to C). But in most other languages, e.g., Java, C#, or Haskell, an arrow is used between parameters and values. Using the C# `=>` might be better than the Java/Haskell `->` because `->` is already used for member lookup on references in C. Therefore, `sin` and `atan2` can be aliased as follows:

```C
f # double => double;
f = sin;

g # (double, double) => double;
g = atan2;
```

### The equivalent type to `T (*f)();`

This is a problem that our system has, the old system has functions that can take any number and type of arguments, as well as functions with defined types for only the first few elements, and then varargs (e.g., `printf`).

We will use the term `*` to represent these, due to its standard use as a glob.

The equivalent of

```C
void (*f)();
void (*g)(int x, ...)
```

is then

```C
f # * => void;
g # (int, *) => void;
```

## Modifiers

C types aren't just about names, pointers, arrays, and functions. They can also have modifiers, such as `const`, `restrict`, or `register`.

These are simply placed before a type.

## Precedence

Since arrays are basically surrounded in parentheses, they don't create an ambiguity. Because `=>` is an infix operator, and infix operators generally have lower precedence than prefix operators, we should have `& A => B` \\(\equiv\\) `(&A) => B`.

Since C has no currying, there's no obvious way to disambiguate `A => B => C`. Applying the equivalence `A => (B => C)` isn't particularly relevant since C functions generally don't return a function, and the equivalence `(A => B) => C` violates an existing convention on grouping of arrows.

Therefore, `A => B => C` with no parentheses should be a syntax error.

Modifiers have lower precedence than `&` but greater precedence than `=>`.

Therefore, `printf` is declared as

```C
printf # (const restrict &char, *) => void;
```

## Putting it all together.

Let's take the absolutely awful function declaration (which I believe is actually not even valid C because of the `char* const`):

```C
char *(*x(int*, const char*, char* const))[5]();
```

and convert to our beautiful format:

```C
x # (&int, &(const char), const &char, &char) => &{* => &char, 5};
```

OK, so a bit longer, but hopefully a lot more comprehensible!
