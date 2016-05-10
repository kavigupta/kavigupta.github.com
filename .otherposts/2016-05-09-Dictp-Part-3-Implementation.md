---
layout: post
title: Dictp Part 3: Implementation
comments: True
---

For [Dictp](/2015/12/30/Dictp-Part-2) to be at all useful, we need to be able to write an interpreter. This seems like a good introduction to Haskell's Parser libraries.

## An Abstract Syntax Tree for Dictp

We know that in Dictp, we have a number of constructs. I would normally diagram a context-free grammar, but Haskell's `data` declaration does more or less the same thing.

```haskell
```

In other words, a DictpAST can be a symbol, a literal string, a literal dictionary, a test that a string contains something, a get expression, and a set expression.

We can implement a parser for the grammar described in the previous article as follows:

```haskell


```