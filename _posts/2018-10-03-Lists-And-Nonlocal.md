---
layout: post
title: Lists and Nonlocal in Python
comments: True
---

## Mutable closures

A closure is a function with a parent frame that contains some data. If the parent frame can be modified, the function is said to be mutable.

For example, take the following doctest:

```python
>>> c, d = make_counter(), make_counter()
>>> c()
0
>>> c()
1
>>> c()
2
>>> [c(), d(), d(), d(), c()]
[3, 0, 1, 2, 4]
```

The `c` is called multiple times and returns a different value every time; but its state isn't global as there is the `d` function which also counts up but on its own tineline.

The Python programming language allows for the creation of mutable closures in two different ways: one that is traditionally considered "functional" and one that is traditionally considered "object-oriented". Let's take a look at them now:

<!-- end excerpt -->

## Nonlocal: Functional mutable closures

The most obvious way to implement the `make_counter` function is as follows:

```python
def make_counter():
    current_value = 0
    def c():
        nonlocal current_value
        result = current_value
        current_value += 1
        return result
    return c
```

The line `nonlocal current_value` ensures that the `current_value` variable assigned in the `c` frame refers to the value from the parent frame. Thus we have shared state (the `current_value` variable) which can be modified, the necessary components of a closure.

## Lists: Object-oriented mutable closures

But do we really need anything that complicated? What if we did this?

```python
def make_counter():
    return [6, 5, 4, 3, 2, 1, 0].pop
```

Every time we call the resulting function, we get 0, 1, 2, 3, 4, 5, and then 6. This isn't quite right, as it doesn't go on forever, but it's definitely a mutable function, and we never used nonlocal. What's going on here? Let's unroll the function somewhat:

```python
def make_counter():
    lst = [6, 5, 4, 3, 2, 1, 0]
    def c():
        return lst.pop()
    return c
```

Here we have that we can modify `lst` from the parent frame without assigning to it, since the *contents* of `lst` rather than the variable itself is mutated. We can exactly duplicate the original `make_counter` function as such:

```python
def make_counter():
    current_value = [0]
    def c():
        result = current_value[0]
        current_value[0] += 1       # looks like an assignment
        return result
    return c
```

Now, this looks like there's an assignment to `current_value` on the commented line, but in fact, `current_value[0] += 1` is equivalent to `current_value[0] = current_value[0].__iadd__(1)`, which is equivalent (for numbers) to `current_value[0] = current_value[0] + 1`, and that is equivalent to `current_value.__setitem__(0, current_value[0] + 1)`, which is not in fact an assignment.

## Complications: Nonlocal on lists

Let's say we write a few alternatives for `make_counter`:

```python
def make_counter_append():
    current_value = [0]
    def c():
        result = current_value[-1]
        current_value.append(current_value[-1] + 1)
        return result
    return c

def make_counter_extend():
    current_value = [0]
    def c():
        result = current_value[-1]
        current_value.extend([current_value[-1] + 1])
        return result
    return c

def make_counter_plus_equals():
    current_value = [0]
    def c():
        result = current_value[-1]
        current_value += [current_value[-1] + 1]
        return result
    return c
```

Interestingly, the first two work but the last does not. Why? Aren't `a += b` and `a.extend(b)` equivalent? Actually, they're *almost* equivalent. `a += b` is equivalent to `a = a.__iadd__(b)`, where `__iadd__` is a special function that you can choose to implement on your class. In the case of Python, `list.__iadd__` is equivalent to `list.extend` except that it returns a reference back to the same list.

Therefore, if you want to use `a += b` where `a` is a list, you thus need either to make `a` nonlocal or use `a.extend(b)` instead.
