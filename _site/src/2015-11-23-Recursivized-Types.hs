{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

import Prelude(Show, show, (++))

data Singleton = Single
    deriving Show

data Void

data Bool = False | True
    deriving Show

data Wrapper a = Wrap a
    deriving Show

data Pair a b = ConsPair a b
    deriving Show

data TwoOfSame a = ConsTwoOfSame a a

data Maybe a = Nothing | Just a
    deriving Show

data Nat = Zero | Succ Nat
    deriving Show

data Stream a = ConsStream a (Stream a)
    deriving Show

trues = ConsStream True trues
falses = ConsStream False falses
alternating = ConsStream True (ConsStream False alternating)

elementAt (ConsStream value rest) Zero = value
elementAt (ConsStream value rest) (Succ x) = elementAt rest x

data List a = Nil | Cons a (List a)
    deriving Show

exampleList = Cons True (Cons False (Cons False Nil))

data Shape f = Structure (f Singleton)

data NullOf f = Null (f Void)

data Recursive f = Recurse (f (Recursive f))

instance (Show (f (Recursive f))) => Show (Recursive f) where
    show (Recurse x) = "Recurse (" ++ show x ++ ")"

zero' = Recurse Nothing
one' = Recurse (Just zero')
two' = Recurse (Just one')
three' = Recurse (Just two')

zero = Zero
one = Succ Zero
two = Succ one
three = Succ two

omega' = Recurse (Just omega')
omega = Succ omega

_ < Zero        = False
Zero < Succ _   = True
Succ x < Succ y = x < y

data Constant a = Const
    deriving (Show)
data Empty a

single = Single
single' = Recurse Const

wrappedRecurse = Recurse (Wrap wrappedRecurse)

trues' = Recurse (ConsPair True trues')
falses' = Recurse (ConsPair False falses')
alternating' = Recurse (ConsPair True (Recurse (ConsPair False alternating')))

data PreList a self = PreList (Maybe (Pair a self))
    deriving (Show)

type RecurseList a = Recursive (PreList a)

exampleList'
    = Recurse (PreList (Just (ConsPair
        True
        (Recurse (PreList (Just (ConsPair
            False
            (Recurse (PreList (Just (ConsPair
                False
                (Recurse (PreList Nothing)))))))))))))

data BinaryTree a = BNode (BinaryTree a) (BinaryTree a) | BLeaf a
data ListTree a = ListNode a (List (ListTree a))

--------ANSWERS TO EXERCISES--------

data PreBinaryTree a self = PBNode self self | PBLeaf a
--- Recursive (PreBinaryTree a) === BinaryTree a

data PreListTree a self = PListNode (Pair a (List self))
    -- equivalent to: data PreListTree a self = PListNode a (List self)
--- Recursive (PreListTree a) === ListTree a

{-
    General solution:
        Add a self type parameter.
        Replace any recursive calls with self.
-}


