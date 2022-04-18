---
layout: post
title: Infix Notation in Scheme
comments: True
---

Scheme is well known as a very simple programming language. The general concept behind scheme is that it is composed of S-expressions, or expressions of the form `(operator operand1 operand2 ... operandn)`, which can be nested.

This makes scheme relatively simple to parse and evaluate, however it does lead to some strange compromises on the style of programming. For example, rather than writing `12 * (2 + 3)`, you have to write `(* 12 (+ 2 3))`. And in a way, this is elegant in its simplicity.

But simplicity is overrated.

<!-- end excerpt -->

## Homoiconicity, or writing a scheme program to write a scheme program

Scheme has this cool property of being a homoiconic language. This means that you can write scheme code that writes and modifies scheme code.

For example, take this function, which replaces the first element form a scheme expression with a `+`:

```
(define (replace-with-plus code)
	(cons '+ (cdr code)))
```

Note that we just treat the code like any other scheme list. To call this function, we can just run

```
scm> (replace-with-plus '(- 2 3))
(+ 2 3)
```

Now, if we want to actually run this program, we can wrap it in an `eval` call as such:

```
scm> (eval (replace-with-plus '(- 2 3)))
5
```

## Macros

You'll note that in the above, we have to quote the arguments and then eval the result. It turns out that there's a construct in Scheme that will do this for you! Using the same replacing with + function, we can define

```
(define-macro (replace-with-plus code)
	(cons '+ (cdr code)))
```

Note that this is the same definition, except that we have replaced `define` with `define-macro`. Now, Scheme handles the quoting of the arguments and the evaluation of the result for us as such:

```
(replace-with-plus (- 2 3))
5
```

Pretty nifty, huh!

## Infix notation

OK, but the post's title is infix notation. Let's say we want to be able to write `(12 * (2 + 3))` in scheme and have it evaluate the way it would in Python. To accomplish this, let's define a function (not a macro for now) that takes in `(12 * (2 + 3))` and converts it into the scheme form `(* 12 (+ 2 3))`. First off, we need to identify which things are in fact infix notation:

```
(define (infix? code)
	(and
		(list? code)
		(= (length code) 3)
		(member (cadr code) '(+ * - /, >, <, >=, <=, =))))
```

The idea here is that we check that the given piece of code is a list with 3 elements, and that its middle element is an operator. Now, let's write the code to convert infix to prefix notation.

```
(define (rearrange code)
	(list (cadr code) (car code) (caddr code)))
```

Where this code just places the elements in second, first, third order.

Now, we can put it together with a recursive function:


```
(define (to-prefix-func code)
	(cond
		((infix? code) (to-prefix-func (rearrange code)))
		((list? code) (map to-prefix-func code))
		(else code)))
```

The idea behind this code is that if something is infix, we should rearrange it, then call ourselves to finish up the task, if something is a call expression, we should just look at every element, and otherwise, we're at a base case and can just return.

Let's try this out on our example:

```
scm> (to-prefix-func '(12 * (2 + 3)))
(* 12 (+ 2 3))
```

It works!

Finally, what we really want is a macro. We can just define one directly:

```
(define-macro (to-prefix code) (to-prefix-func code))
```

Sadly we can't just `(define-macro to-prefix to-prefix-func)` because of a technicality in the scheme language. Anyway, now we can run our scheme code:

```
scm> (to-prefix (12 * (2 + 3)))
60
```

And there you have it. Scheme's power might be in its simplicity but with great power comes the ability to create complexity. Obviously, the usual disclaimer applies: *don't write code like this*, but if you do, do it with macros!

## The code

A fully runnable version of this is below: try copy-pasting it into [61A's scheme interpreter](http://scheme.cs61a.org) and check it out! (There are a few helpful functions defined at the end that we just kinda assumed the existence of above).

```
(define (infix? code)
	(and
		(list? code)
		(= (length code) 3)
		(member (cadr code) '(+ * - /, >, <, >=, <=, =))))

(define (rearrange code)
	(list (cadr code) (car code) (caddr code)))

(define (to-prefix-func code)
	(cond
		((infix? code) (to-prefix-func (rearrange code)))
		((list? code) (map to-prefix-func code))
		(else code)))

(define-macro (to-prefix code)
	(to-prefix-func code))

(define (member elem lst)
	(cond
		((null? lst) #f)
		((eq? (car lst) elem) #t)
		(else (member elem (cdr lst)))))

(define (cadr lst) (car (cdr lst)))
(define (cddr lst) (cdr (cdr lst)))
(define (caddr lst) (car (cddr lst)))
```
