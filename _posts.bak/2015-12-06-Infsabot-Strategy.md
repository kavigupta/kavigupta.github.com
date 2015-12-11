preprocess:
    pass ../_scripts/codetosources.py
    replace "haskellcomment" -> "haskell"
    replace "<!--_-->" -> "<!--_-->"
---
layout: post
title: Infsabot Strategy Part 1
comments: True
---

dump: haskell as hs

OK, so what is this _Infsabot_? Infsabot is a game I designed for people to write programs to play. Basically, it's like chess, except where each piece acts independently of each other.

## The Game

There is a square board where each square is either empty or contains material. The game pieces, robots, require material to stay alive and perform tasks. At each turn, every robot reports which of the following it wants to do:

 - Noop: The robot does nothing
 - Die: The robot dies
 - Dig: The robot digs to try to extract material
 - Move: The robot can move in one of the four compass directions
 - Fire: The robot can fire a quick-moving projectile (e.g., travel time = 0) in a compass direction
 - Message: The robot can send a quick-moving message in a compass direction
 - Spawn: The robot can create a new robot with its own initial material and appearance

To decide what to do, a robot has access to two things:

 - A picture of the positions around it, to a given distance
    - Whether or not the spot contains material
    - Whether or not the spot contains a robot, and it's appearance (_not_ it's team)
 - A snapshot of its own hard drive, which is a massive relational database between Strings and Strings
 - A list of all messages received

Crucially, there is no way to tell whether a robot one sees is on one's own team or not. This must be accomplished through sending an receiving messages.

## Robot strategies

A robot strategy can be expressed as a type

```haskell
type RobotProgramResult = (RobotAction, InternalState)
type RobotProgram = KnownState -> RobotProgramResult
```

To create a baseline for human-written strategies, I am going to use an adaptive algorithm to try to generate a strategy. This basic algorithm won't take into account any of the "human" factors like messages and internal state.

I will reify the concept of a (non-recursive) robot program by representing it as a long expression tree. The basic concept is something like this:

```haskell
data ActionType = TDie | TNoop | TFire | TDig | TMoveIn | TSpawn

data RP =
    DefaultRepresentation ActionType |
    ModifyMaterial ExprInt RP |
        -- Applies to Fire, Spawn
    ModifyDirection ExprDir RP |
        -- Applies to Move, Spawn, Fire, Send
    ModifyAppearance ExprInt RP |
        -- Applies to Spawn. RGB treated as a single integer.
    IfRP ExprBool RP RP
```

So, `DefaultRepresentation` describes a default representation of the given `ActionType`. `Modify*` modifies the given field, if it exists, of the given action. The various `Expr*` types represent expression tree types, given below.

```haskell
data ExprInt =
    ConstInt Int |
    RobAppear ExprPath ExprInt | -- Coordinate; Default if no robot
    MaterialLevel | -- Current material level
    Age | -- Current Age
    (:*) ExprInt ExprInt |
    (:+) ExprInt ExprInt |
    (:-) ExprInt ExprInt |
    (:/) ExprInt ExprInt |
    Mod ExprInt ExprInt |
    IfInt ExprBool ExprInt ExprInt |
    SwitchInt ExprDir ExprInt ExprInt ExprInt ExprInt

-- Basically [ExprDir]
    -- Written like this to make the tree more explicit
data ExprPath =
    Here |
    Offset ExprDir ExprPath

data ExprDir =
    ConstDir RDirection |
    IfDir ExprBool ExprDir ExprDir

data ExprBool =
    ConstBool Bool |
    CanSee ExprPath |
    MaterialAt ExprPath | -- False if can't see that far
    RobotAt ExprPath | -- False if can't see that far
    EqualInt ExprInt ExprInt |
    GreaterInt ExprInt ExprInt |
    EqualDir RDirection RDirection |
    Not ExprBool |
    (:&) ExprBool ExprBool |
    (:|) ExprBool ExprBool
```

OK, so that's a lot of mutually recursive trees. I'm going to skip the boring implementation of `eval` for each of these trees, but here's a simple interface for that purpose:

```haskell
class Evaluable given from to where
    eval :: given -> from -> to

instance Evaluable KnownState ExprInt Ratio where
    -- Evaluates to a Ratio, or fraction, for precision reasons.
    -- Round to get an actual integer
    ...
instance Evaluable KnownState ExprBool Bool where
    ...
instance Evaluable KnownState ExprDir RDirection where
    ...
instance Evaluable KnownState ExprPath [RDirection] where
    ...
```

## Constructing Trees

OK, so to construct some trees. Here's a simple move and dig strategy that tries to maximize the digginess of a given robot.

```haskell
digStrategy
    = IfRP (MaterialAt Here)
        (DefaultRepresentation TDig)
        (ModifyDirection
            (IfDir
                (MaterialAt
                    (Offset (ConstDir N) Here))
                (ConstDir N)
                (IfDir
                    (MaterialAt
                        (Offset (ConstDir E) Here))
                    (ConstDir E)
                    (IfDir
                        (MaterialAt
                            (Offset (ConstDir S) Here))
                        (ConstDir S)
                        (ConstDir W))))
            (DefaultRepresentation TMoveIn))
```

That's some damn good looking Scheme---ahem, Infsabot Strategy DSL!

Anyway, next time I'll cover how to generate code in the form of this structure. Hopefully, the computer can do most of the heavy lifting!

Note: I made a few corrections to `digStrategy` because it wasn't exactly compiling.
