preprocess:
    include
---
layout: post
title: Either in Java
comments: True
---

## Either

Either is a construct in algebraically-typed languages that allows for returning multiple types from the same function. It is defined as

```haskell
data Either a b = Left a | Right b
``` 

This declaration can be read as: for any types `a` and `b`, `Either a b` is a type that has values of the form `Left x`, where `x` is of type `a` or of the form `Right y`, where `y` is of type `b`.

Either is beneficial because it, allong with structures, allows the creation of algebraic types. For example, a Haskell `Maybe`, which is a nullable type, can be expressed as:

```haskell
type Maybe a = Either () a
null = Left () 
just = Right
```

Which says that a nullable type is either `null` or `just x`.

We can also implement a linked list as:

```haskell
data List a = Either () (a, List a)
cons x y = Right (x, y)
nil = Left ()
```

Which says that a list is either `nil`, the end of a list, or `cons`, which joins the head of a list, an element, to the tail fo a list, another list.

## Polymorphic Either

The standard way that Java implements `Either` is [to use polymorphism](http://stackoverflow.com/questions/26162407/is-there-an-equivalent-of-scalas-either-in-java-8). While this does allow for the implementation of a type-safe fold, e.g., a function with signature

```java
public static T fold(Either<L, R> value, Function<L, T> overLeft, Function<R,T> overRight);
```

However, unpacking a value can often be a very complex process, and polymorphic either seems like a reinvention of a basic construct of the Java language, that was, in fact, built in.

## Error-based Either

We can define a function which returns two types in java using exceptions:

We just need to define the exception type

```java
include "../_resources/2016-04-15/Left.java"
```

and then the multiple-return type function class:

```java
include "../_resources/2016-04-15/MRTFunction.java"
```
