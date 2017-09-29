preprocess:
    replace "IMAGE: ([^;\n]+);?(.*)" -> "<center><img src=\"/logic-game/eg/\\1.png\" \\2 /></center>"
    replace "<!--a*-->" -> ""
---
layout: post
title: Logical Game Diagrams
comments: True
---

## A Basic Game

Let's look at a simple game with two players: the Expositor and the Adversary. Here's an example of one of these games:

IMAGE: inequality-adv-first

The way it's played is that the expositor makes the \\(\exists\\) moves, and the adversary makes the \\(\forall\\) moves, where each move \\(a : B\\) involves picking some value from \\(a\\) out of the valid move set \\(B\\).

A round is won by the expositor if the final expression is true (in this case, if \\(x \leq y\\)). It is won by the adversary if the final expression is false. As it turns out, there is always an optimal strategy for either the expositor or adversary that yields a guaranteed win. We call games that the expositor can win E-games and those the adversary can win A-games. The above game is an E-game as the expositor can always play \\(y = x + 1\\), thus guaranteeing a win.

## Order of Moves

The order of moves matters; in general there can be a "move last" advantage but never a "move first" advantage. The above game shows how there is a "move last" advantage. The opposite game, as described by the following diagram,

IMAGE: inequality-exp-first

is an A-game since the adversary can win in all cases simply by setting \\(x = y + 1\\). Thus, it turns out that in this game whoever goes second wins. The second move advantage occurs because the second mover can decide what to do knowing what the first person played. In this case, the game is effectively "name the bigger number," which is an impossible game to win if you move first but trivial if you move second.

There's never a first-move advantage because if you go second, you could make the same move that you would have had you gone first, and force a win in any case.

You might think that the second move advantage has to do with the infinite number of moves. In the biggest number game, if we take a finite number of moves, then there would be an obvious win for the \\(\exists\\) player: the maximum. However, we can set up a simple game with a second move advantage:

IMAGE: bool-eq-adv-first

In this case, we have that the expositor can win by simply playing the same thing the adversary played; thus making this an E-game. The opposite is an A-game as the adversary can always play the opposite of what the expositor played:

IMAGE: bool-eq-exp-first

## Binary Operators

Giving a choice of two moves is common enough that we add two "binary choice" operators: one for each player. The above game can be stated as such (with the adversary playing first):

IMAGE: xnor-adv-first

The operator \\(\wedge\\) is one in which the adversary can choose a direction to move, and the operator \\(\vee\\) is one in which the expositor can choose a direction. In this case, we use \\(\top\\) to represent an automatic win for the expositor and \\(\bot\\) to represent an automatic win for the adversary.

## Balance Player

So far, we have only two players: the expositor who attempts to make the final expression true, and the adversary, who attempts to make it false. However, we can also add a balance player, who seeks a balance between true and false statements.

We thus add a balance connective:

IMAGE: bigger-number-simul.png

There is no winning strategy for either player here because
