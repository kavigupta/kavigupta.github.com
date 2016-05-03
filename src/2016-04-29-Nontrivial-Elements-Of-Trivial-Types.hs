{-# LANGUAGE RankNTypes #-}
import Prelude hiding (undefined, error, succ)

data Vehicle = Car { vin :: Integer, licensePlate :: String }
        | Bike

data Void

void :: Void
void = void

void2, void3 :: Void
void2 = undefined
void3 = error "any string here"

undefined = undefined
error _ = undefined

undefined :: forall a. a

un0 = undefined + 2
un1 = sin undefined
un2 = let f x = x * 2 in f undefined
un3 = head [undefined, 2, 3, 4]
un4 = undefined == 2 -- (==) is just a function

const_ignores_arguments = const 2 undefined
head_only_requires_head = head [2, undefined, undefined]
len_of_value = length [undefined, undefined]
variable_binding_doesn't_discriminate = let f x = 2 in f undefined

undefined_can_be_in_a_list = [undefined] -- (length, tail, \[x] -> 2)
a_tuple_of_undefined = (,) undefined undefined -- (\(x,x) -> 2)
list_containing_undefined = tail [2, undefined, 2, 4] -- (length, !! 1)
wrapper_type = return undefined :: Maybe a -- (\(Just x) -> 2)
evaluation_to_undefined_is_local = map (*2) [undefined, undefined] -- (length, tail . tail)

data Wrap = W Wrap

data Nat = Z | S Nat
f :: Nat -> Wrap -> ()
f Z _ = ()
f (S n) (W x) = f n x

--last_value = W (W (W (W (W (W (W (W (W (W (W (W (W ...))))))))))))
last_value :: Wrap
last_value = W last_value

class Peano a where
    zero :: a
    succ :: a -> a

peano_struct :: (Peano a) => (a, a -> a)
peano_struct = (zero, succ)

one, two, three, four, five :: (Peano a) => a
one = succ zero
two = succ one -- = succ (succ zero)
three = succ two -- = succ (succ (succ zero))
four = succ three -- = succ (succ (succ succ (zero)))
five = succ four -- = succ (succ (succ (succ succ (zero))))

alt_zero, alt_one, alt_two, alt_three :: (Peano a) => a
alt_zero = undefined
alt_one = succ alt_zero -- = succ undefined
alt_two = succ alt_one -- = succ (succ undefined)
alt_three = succ alt_two -- = succ (succ (succ undefined))

omega :: (Peano a) => a
omega = succ omega

instance (Peano Nat) where
    zero = Z
    succ = S

instance (Peano Integer) where
    zero = 0
    succ = (+1)

omega_int :: Integer
omega_int = omega_int + 1

