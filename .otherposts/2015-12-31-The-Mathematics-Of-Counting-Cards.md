preprocess:
    pass ../_scripts/codetosources.py
    replace "<!--_-->" -> ""
---
layout: post
title: The Mathematics of Counting Cards
comments: True
---

dump: haskell as hs

OK, before you all ban me from Nevada or something, I mean *literally* counting cards. Like \\(N\\)-card pickup.

## The problem

So we have a deck of cards, and we don't know if we've lost some or not. However, what we do know is that we haven't gained any.

In other words, we know that there the true number of cards \\(n \in [0, N]\\), where \\(N\\) is the total number of cards in a full deck (not necessarily 52, e.g., if we're playing BS with 3 decks).

we can count the cards, but for every card we count there is some probability \\(p\\) of skipping a card or double counting a card accidentally. Unfortunately, we don't know what either of these probabilities is.

Anyway, we count the cards and get the number \\(\mu\\).

For which \\(\mu\\) should we be most certain that the deck is full?

## Intuitive exploration

OK, so we know that the higher the card count, the higher the probability that deck is full, all things being equal.

However, if we get \\(\mu > N\\), we should start to trust our counting skills less.

Overall, it seems that we have two variables: \\(p\\) and actual card count, or \\(n\\), and only one "equation", the sample that we managed to take. This seems like a hard problem, but let's look at the actual math a little before giving up.

## Card Distribution

So the total number of cards is \\(n\\). Each card \\(c_i \tilde \\{0:p, 1:1-2p, 2: p\\}\\). Therefore, the distribution of values of \\(\mu\\) is the sum of \\(n\\) independent probability variables with that distribution.

Looking at the error in each case, \\(e_i\\), we get the formula

\\[e_i \tilde \\{ -1: p, 1: p, 0: 1 - 2p\\}\\]

For some total error \\(\epsilon\\), we need to have some number of over-counts \\(a\\), undercounts \\(b\\), and correct counts \\(c\\) such that

\\[a + b + c = n\\]
\\[a - b = \epsilon\\]

Combining these equations, we then get that

\\[2a + c = n + \epsilon\\]

With the restriction that \\(a + c \leq n\\), we find that \\(n-c = 2a - \epsilon \geq a\\), or that \\(a \geq \epsilon\\). In other words, the number of over-counts must be at least the error. Not earth-shattering stuff. Anyway, we have

\\[p(\epsilon) = \sum_{a=\max{\epsilon, 0}}^N P(a)\\]

where \\(P(a)\\) is the probability of getting \\(a\\) over-counts. We have in this situation \\(b = a - \epsilon\\) and \\(c = N + \epsilon - 2a\\)

\\[P(a) = \{N \choose \{a,(a - \epsilon)\}\}p^a p^{a-\epsilon} (1-2p)^{N + \epsilon - 2a}\\]

Putting this together, we have

\\[p(\epsilon) = \sum_{a=\max{\epsilon, 0}}^N\{N \choose \{a,(a - \epsilon)\}\}p^{2a-\epsilon} (1-2p)^{N + \epsilon - 2a}\\]

The following is a haskell implementation of the algorithm given above:

```haskell

factorial :: (Integral a) => a -> a
factorial n
    | n >= 0    = product [1..n]
    | otherwise = error "Negative factorial is not defined. Perhaps you meant to use the gamma function"

probabilityOfError :: (Fractional a) => Integer -> Integer -> a -> a
probabilityOfError nError nCards p = sum $ map probabilityGiven [0..nCards]
    where
    probabilityGiven overcounts
            = (fromIntegral $ trinomialCoefficient nCards undercounts overcounts)
                * p ^^ overcounts
                * p ^^ undercounts
                * (1 - 2 * p) ^^ (nCards - overcounts - undercounts)
        where
        undercounts = overcounts - nError

trinomialCoefficient :: (Integral a) => a -> a -> a -> a
trinomialCoefficient n a b
    | a < 0 || b < 0 || a + b > n
        = 0
    | otherwise
        = factorial n `div` (factorial a * factorial b * factorial (n - a - b))
```

As a quick verification that it works, we can sum up the values of this function for every possible error (from `-52` to `52` when you have `52` cards).

```interactive
*Main> (sum $ map (\x -> probabilityOfError x 52 0.25) [-52..52]) :: Rational
1 % 1
```

As you can see, the probability is 1 as expected.

## Going backwards

OK, so we know that the probability of getting \\(\mu\\) given some value \\(n\\) is

\\[P(\mu | n) = p(\epsilon + n)\\]

Applying [Bayes' rule](https://en.wikipedia.org/wiki/Bayes%27_theorem) and plugging in \\(n = N\\), i.e., a full deck, we get

\\[\frac{P(\mu | N)}{P(\mu)} = \frac{P(N | \mu)}{P(N)}\\]

Now, we have just introduced two new quantities, the overall probability of \\(\mu\\) and the overall probability of \\(N\\).

The overall probability of \\(\mu\\) is fairly simple to calculate: it is the sum of \\[P(\mu | n)\\] over all \\(n\\).

The overall probability of \\(N\\) on the other hand... well, it's the sum of thee value we want over all \\(\mu\\). This is not really possible to calculate. However, it _is_ constant in \\(\mu\\), which is the one actual variable in our situation, so we're fine there. On a deeper level, we can think of \frac{P (N | \mu)}{P(N)} as a reflection of how drawing \\(\mu\\) affects our perception of how likely \\(P(N)\\) is.

\\[E_\mu = \frac{P(N | \mu)}{P(N)} = \frac{P(\mu | N)}{P(\mu)}\\]

Here are some Haskell functions useful for calculating this value.

```haskell
probMuGivenN :: (Fractional a) => Integer -> Integer -> a -> a
probMuGivenN sample cards = probabilityOfError (sample-cards) cards

probMu :: (Fractional a, Ord a) => a  -> Integer -> a -> a
probMu epsilon sample p = sum $ map fst $ takeWhile valid withNext
    where
    probabilities = map (\cards -> probMuGivenN sample cards p) [0..]
    withNext = zip probabilities (tail probabilities)
    valid (curr, next)
        | curr <= next   = True
        | otherwise     = next > epsilon    

effect :: (Fractional a, Ord a) => Integer -> Integer -> a -> a
effect sample cards p = probMuGivenN sample cards p / probMu 1e-100 sample p
```

## Finding \\(p\\)

OK, so we've been treating \\(p\\) like a parameter this whole time, but in reality, it is a function of what you get, as we analyzed in the section on intuition.

So let's look at slicing that distribution another way.

If we hold \\(n\\) as a parameter, we can find the distribution of


P(mu = mu0) = f(N, mu0, n, p)

P(n = N | mu = mu0) = P(mu = mu0 | n = N) \frac{P(n = N)}{P(mu = mu0)}
                    = \frac{f(N, mu0, N, p)}{\sum_{0 \leq n \leq N} f(N, mu0, n, p)} P(n=N)

P(n = n0) = 
