{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
-- |
-- Module      : Types.Number.Nat
-- Copyright   : Alexey Khudyakov
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability   : unstable
-- Portability : unportable (GHC only)
--
--
-- This is type level natural numbers. They are represented using
-- binary encoding which means that reasonable large numbers could be
-- represented. With default context stack depth (20) maximal number
-- is 2^18-1 (262143).
--
-- > Z           = 0
-- > I Z         = 1
-- > O (I Z)     = 2
-- > I (I Z)     = 3
-- > O (O (I Z)) = 4
-- > ...
--
-- It's easy to see that representation for each number is not
-- unique. One could add any numbers of leading zeroes:
--
-- > I Z = I (O Z) = I (O (O Z)) = 1
--
-- In order to enforce uniqueness of representation only numbers
-- without leading zeroes are members of Nat type class. This means
-- than types are equal if and only if numbers are equal.
--
-- Natural numbers support comparison and following operations: Next,
-- Prev, Add, Sub, Mul. All operations on numbers return normalized
-- numbers.
--
-- Interface type classes are reexported from Types.Number.Classes
module Types.Number.Nat ( -- * Natural numbers
                          I
                        , O
                        , Z
                        , Nat(..)
                          -- * Template haskell utilities
                          -- $TH
                        , natT
                        , module Types.Number.Classes
                        ) where

import Language.Haskell.TH

import Types.Number.Classes
import Types.Number.Nat.Types
import Types.Util

splitToBits :: Integer -> [Int]
splitToBits 0 = []
splitToBits x | odd x     = 1 : splitToBits rest
              | otherwise = 0 : splitToBits rest
                where rest = x `div` 2

-- $TH
-- Here is usage example for natT:
--
-- > n123 :: $(natT 123)
-- > n123 = undefined
--
-- This require type splices which are supprted by GHC>=6.12.

-- | Create type for natural number.
natT :: Integer -> TypeQ
natT n | n >= 0    = foldr appT [t| Z |] . map con . splitToBits $ n
       | otherwise = error "natT: negative number is supplied"
  where
    con 0 = [t| O |]
    con 1 = [t| I |]
    con _ = error "natT: Strange bit nor 0 nor 1"

----------------------------------------------------------------

-- | Type class for natural numbers. Only numbers without leading
-- zeroes are members of this type class.
class TypeInt n => Nat n

instance                  TypeInt       Z   where toInt _ = 0
instance                  TypeInt    (I Z)  where toInt _ = 1
instance TypeInt (O n) => TypeInt (O (O n)) where toInt n = 0 + 2 * toInt (cdr n)
instance TypeInt (O n) => TypeInt (I (O n)) where toInt n = 1 + 2 * toInt (cdr n)
instance TypeInt (I n) => TypeInt (O (I n)) where toInt n = 0 + 2 * toInt (cdr n)
instance TypeInt (I n) => TypeInt (I (I n)) where toInt n = 1 + 2 * toInt (cdr n)

instance                  Nat    Z
instance TypeInt (O n) => Nat (O n)
instance TypeInt (I n) => Nat (I n)
-- Error reporting
class    Number_Is_Denormalized a
instance (Number_Is_Denormalized Z) => TypeInt (O Z) where
  toInt = error "quench warning"

----------------------------------------------------------------
-- Number normalization

-- Add trailing zero bit to number. It's added only if number is not
-- equal to zero. Actual normalization is done here.
type family   Add0Bit n :: *
type instance Add0Bit    Z  = Z
type instance Add0Bit (a b) = (O (a b))

type instance Normalized    Z  = Z
type instance Normalized (I n) = I (Normalized n)
type instance Normalized (O n) = Add0Bit (Normalized n)

----------------------------------------------------------------
-- Show instances.
-- Nat contexts are used to ensure correctness of numbers.
instance              Show    Z  where show _ = "[0:N]"
instance Nat (O n) => Show (O n) where show n = "["++show (toInt n)++":N]"
instance Nat (I n) => Show (I n) where show n = "["++show (toInt n)++":N]"

----------------------------------------------------------------
-- Next number.
-- Number normalization is not required.
instance                        NextN    Z  where type Next Z = I Z
instance (Nat (I n),NextN n) => NextN (I n) where type Next (I n) = O (Next n)
instance (Nat (O n),NextN n) => NextN (O n) where type Next (O n) = I n

----------------------------------------------------------------
-- Previous number.
-- Normalization isn't requred too. It's done manually in (I Z) case.
instance                             PrevN    (I Z)  where type Prev (I Z)     = Z
instance (Nat (O n), PrevN (O n)) => PrevN (O (O n)) where type Prev (O (O n)) = I (Prev (O n))
instance (Nat (O n), PrevN (O n)) => PrevN (I (O n)) where type Prev (I (O n)) = O (O n)
instance (Nat (I n), PrevN (I n)) => PrevN (O (I n)) where type Prev (O (I n)) = I (Prev (I n))
instance (Nat (I n), PrevN (I n)) => PrevN (I (I n)) where type Prev (I (I n)) = O (I n)


----------------------------------------------------------------
-- Comparison

-- Join compare results. a is result of comparison of low digits b is
-- result of comparion of higher digits.
class JoinCompare a b where
    type Join a b :: *

instance JoinCompare IsLesser  IsEqual   where type Join IsLesser  IsEqual   = IsLesser
instance JoinCompare IsEqual   IsEqual   where type Join IsEqual   IsEqual   = IsEqual
instance JoinCompare IsGreater IsEqual   where type Join IsGreater IsEqual   = IsGreater
instance JoinCompare a         IsLesser  where type Join a         IsLesser  = IsLesser
instance JoinCompare a         IsGreater where type Join a         IsGreater = IsGreater

-- Instances for comparison
instance              CompareN    Z     Z  where type Compare    Z     Z  = IsEqual
instance Nat (O n) => CompareN (O n)    Z  where type Compare (O n)    Z  = IsGreater
instance Nat (I n) => CompareN (I n)    Z  where type Compare (I n)    Z  = IsGreater
instance Nat (O n) => CompareN    Z  (O n) where type Compare    Z  (O n) = IsLesser
instance Nat (I n) => CompareN    Z  (I n) where type Compare    Z  (I n) = IsLesser

instance (Nat (O n), Nat (O m)) => CompareN (O n) (O m) where type Compare (O n) (O m) = Compare n m
instance (Nat (O n), Nat (I m)) => CompareN (O n) (I m) where type Compare (O n) (I m) = Join IsLesser  (Compare n m)
instance (Nat (I n), Nat (O m)) => CompareN (I n) (O m) where type Compare (I n) (O m) = Join IsGreater (Compare n m)
instance (Nat (I n), Nat (I m)) => CompareN (I n) (I m) where type Compare (I n) (I m) = Compare n m

----------------------------------------------------------------
-- Addition
data Carry      -- Designate carry bit
data NoCarry    -- No carry bit in addition

-- Type family which actually implement addtition of natural numbers
type family Add' n m c :: *

-- Recursion termination without carry bit. Full enumeration is
-- required to avoid overlapping instances
type instance Add'    Z     Z  NoCarry = Z
type instance Add' (O n)    Z  NoCarry = O n
type instance Add' (I n)    Z  NoCarry = I n
type instance Add'    Z  (O n) NoCarry = O n
type instance Add'    Z  (I n) NoCarry = I n
-- Recursion termination with carry bit
type instance Add'    Z   Z      Carry = I Z
type instance Add' (O n)  Z      Carry = I n
type instance Add' (I n)  Z      Carry = Next (I n)
type instance Add'    Z  (O n)   Carry = I n
type instance Add'    Z  (I n)   Carry = Next (I n)
-- Generic recursion (No carry)
type instance Add' (O n) (O m) NoCarry = O (Add' n m NoCarry)
type instance Add' (I n) (O m) NoCarry = I (Add' n m NoCarry)
type instance Add' (O n) (I m) NoCarry = I (Add' n m NoCarry)
type instance Add' (I n) (I m) NoCarry = O (Add' n m   Carry)
-- Generic recursion (with carry)
type instance Add' (O n) (O m)   Carry = I (Add' n m NoCarry)
type instance Add' (I n) (O m)   Carry = O (Add' n m   Carry)
type instance Add' (O n) (I m)   Carry = O (Add' n m   Carry)
type instance Add' (I n) (I m)   Carry = I (Add' n m   Carry)

-- Enumeration of all possible instances heads is required to avoid
-- overlapping.
instance (Nat (O n), Nat (O m)) => AddN (O n) (O m) where type Add (O n) (O m) = Add' (O n) (O m) NoCarry
instance (Nat (I n), Nat (O m)) => AddN (I n) (O m) where type Add (I n) (O m) = Add' (I n) (O m) NoCarry
instance (Nat (O n), Nat (I m)) => AddN (O n) (I m) where type Add (O n) (I m) = Add' (O n) (I m) NoCarry
instance (Nat (I n), Nat (I m)) => AddN (I n) (I m) where type Add (I n) (I m) = Add' (I n) (I m) NoCarry
instance (Nat (O n))            => AddN (O n)    Z  where type Add (O n)    Z  = Add' (O n)    Z  NoCarry
instance (Nat (I n))            => AddN (I n)    Z  where type Add (I n)    Z  = Add' (I n)    Z  NoCarry
instance (Nat (O n))            => AddN    Z  (O n) where type Add    Z  (O n) = Add'    Z  (O n) NoCarry
instance (Nat (I n))            => AddN    Z  (I n) where type Add    Z  (I n) = Add'    Z  (I n) NoCarry
instance                           AddN    Z     Z  where type Add    Z     Z  = Add'    Z     Z  NoCarry

----------------------------------------------------------------
-- Subtraction
data Borrow     -- Designate carry bit
data NoBorrow   -- No carry bit in addition

-- Type class which actually implement addtition of natural numbers
type family Sub' n m c :: *

-- Recursion termination without carry bit. Full enumeration is
-- required to avoid overlapping instances
type instance Sub'    Z     Z  NoBorrow = Z
type instance Sub' (O n)    Z  NoBorrow = O n
type instance Sub' (I n)    Z  NoBorrow = I n
-- Recursion termination with carry bit
type instance Sub' (O n)  Z      Borrow = I (Sub' n Z Borrow)
type instance Sub' (I n)  Z      Borrow = O n
-- Generic recursion (No carry)
type instance Sub' (O n) (O m) NoBorrow = O (Sub' n m NoBorrow)
type instance Sub' (I n) (O m) NoBorrow = I (Sub' n m NoBorrow)
type instance Sub' (O n) (I m) NoBorrow = I (Sub' n m   Borrow)
type instance Sub' (I n) (I m) NoBorrow = O (Sub' n m NoBorrow)
-- -- Generic recursion (with carry)
type instance Sub' (O n) (O m)   Borrow = I (Sub' n m   Borrow)
type instance Sub' (I n) (O m)   Borrow = O (Sub' n m NoBorrow)
type instance Sub' (O n) (I m)   Borrow = O (Sub' n m   Borrow)
type instance Sub' (I n) (I m)   Borrow = I (Sub' n m   Borrow)

-- Enumeration of all possible instances heads is required to avoid
-- overlapping.
instance (Nat (O n), Nat (O m)) => SubN (O n) (O m) where type Sub (O n) (O m) = Normalized (Sub' (O n) (O m) NoBorrow)
instance (Nat (I n), Nat (O m)) => SubN (I n) (O m) where type Sub (I n) (O m) = Normalized (Sub' (I n) (O m) NoBorrow)
instance (Nat (O n), Nat (I m)) => SubN (O n) (I m) where type Sub (O n) (I m) = Normalized (Sub' (O n) (I m) NoBorrow)
instance (Nat (I n), Nat (I m)) => SubN (I n) (I m) where type Sub (I n) (I m) = Normalized (Sub' (I n) (I m) NoBorrow)
instance (Nat (O n))            => SubN (O n)    Z  where type Sub (O n)    Z  = Normalized (Sub' (O n)    Z  NoBorrow)
instance (Nat (I n))            => SubN (I n)    Z  where type Sub (I n)    Z  = Normalized (Sub' (I n)    Z  NoBorrow)
instance                           SubN    Z     Z  where type Sub    Z     Z  = Normalized (Sub'    Z     Z  NoBorrow)

-- Error handling
-- instance SubN' Z    Z    Borrow where type Sub' Z  Z      Borrow = I Z
-- instance SubN' Z (O n) NoBorrow where type Sub' Z (O n) NoBorrow = O n
-- instance SubN' Z (I n) NoBorrow where type Sub' Z (I n) NoBorrow = I n
-- instance SubN' Z (O n)   Borrow where type Sub' Z (O n)   Borrow = I n
-- instance SubN' Z (I n)   Borrow where type Sub' Z (I n)   Borrow = O (Sub' Z n Borrow)
-- instance (Nat (O n))            => SubN Z (O n) where type Sub Z (O n) = Normalized (Sub'    Z  (O n) NoBorrow)
-- instance (Nat (I n))            => SubN Z (I n) where type Sub Z (I n) = Normalized (Sub'    Z  (I n) NoBorrow)

----------------------------------------------------------------
-- Multiplication
----------------------------------------------------------------

instance (Nat n)            => MulN n    Z  where type Mul n    Z  = Z
instance (Nat n, Nat (O m)) => MulN n (O m) where type Mul n (O m) = Normalized (O (Mul n m))
instance (Nat n, Nat (O n)) => MulN n (I m) where type Mul n (I m) = Normalized (Add n (O (Mul n m)))
