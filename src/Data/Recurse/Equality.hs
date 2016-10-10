{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilyDependencies #-}

{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Recurse.Equality where

import Data.Type.Equality
import Data.Kind
import GHC.Generics ()
import Data.Recurse
import Data.Recurse.Recursing
import Data.Lifted
import Data.X
import Unsafe.Coerce
import Data.Proxy
import Data.Tree

import Data.Typeable

-- TODO: Make Recurse a newtype to make casting safe!
-- Note: Use these to avoid template haskell for Rec/Dec


-----------------------------------------------------------------------------------

-- | Likely unfinished, but this is will be the interface to the result of this module
class (Req a a b b 'Z ~ eq) => RecurseEq (a :: *) (b :: *) (eq :: Bool) where
  req   :: RecurseL a -> RecurseL b -> Maybe (RecurseL a :~: RecurseL b)
  reqP  :: RecurseL a -> Proxy    b -> Maybe (RecurseL a :~: RecurseL b)

instance (Req a a b b 'Z ~ 'True) => RecurseEq a b 'True where
  req  _ _ = Just . unsafeCoerce $ Refl
  reqP _ _ = Just . unsafeCoerce $ Refl

instance (Req a a b b 'Z ~ 'False) => RecurseEq a b 'False where
  req  _ _ = Nothing
  reqP _ _ = Nothing

-- | Safely cast one recursive datatype to another
rcast :: (Req a a b b 'Z ~ 'True) => RecurseL a -> RecurseL b
rcast = unsafeCoerce




-- | `X`-level `[]`. Could handle being renamed.
data VoidX

-- | Stub instance
instance Show VoidX where
  show _ = "VoidX"


infixr 1 .:
-- | `X`-level `:`
data (.:) (a :: *) (b :: *) = (.:) a b

infixr 2 .$
-- | `X`-level `$`
type family (.$) (a :: *) (b :: *) :: * where
  (.$) (X (c :: k -> k1)) (X (a :: k)) = X (c a)

infixr 0 .||
-- | `X`-level `||`
type family (.||) (a :: *) (b :: *) :: * where
  (.||) (X VoidX) b = b
  (.||)  a        b = a

-- | Recursively unfold a type
type family UnfoldX (a :: *) :: * where
  UnfoldX a = UnfoldXL VoidX (X a)

-- | Recursively unfold a type with given argument list
type family UnfoldXL (l :: *) (a :: *) :: * where
  UnfoldXL l (X (c a)) = UnfoldXL (UnfoldX a .: l) (X c) .|| X c .: (UnfoldX a .: l)
  UnfoldXL l (X  c   ) =                                     X c .:               l

-- | Recursively fold a (X type :. type list)
-- Should have:
-- FoldX (UnFoldX a) == X a
type family FoldX (a :: *) :: * where
  FoldX (X c .:  VoidX  ) =        X c
  FoldX (X c .: (a .: b)) = FoldX (X c .$ FoldX a .: b)

-- | Show an unfolded type in tree form
showx :: ToTree a => a -> String
showx = drawTree . toTree

-- | Print a type unfolded and in tree form
printu :: ToTree (UnfoldX a) => a -> IO ()
printu = putStrLn . showx . (undefined :: a -> UnfoldX a)

-- | See `toTree`
class ToTree (a :: *) where
  -- | Convert an unfolded type into a `Tree` of `String`s
  toTree   :: a -> Tree String

-- | See `ToTree`
class ToForest (a :: *) where
  -- | See `toTree`
  toForest :: a -> Forest String

instance Typeable (X a) => ToTree (X a) where
  toTree x = Node (drop 2 . show . typeOf $ x) []

instance (Typeable (X a), ToForest bs) => ToTree (X a .: bs) where
  toTree x = Node (label x) (toForest (rm x))
    where
      hd :: (X a .: bs) -> X a
      hd _ = undefined
      label = drop 2 . show . typeOf . hd
      rm :: (X a .: bs) -> bs
      rm _ = undefined

instance (ToTree a, ToForest as) => ToForest (a .: as) where
  toForest x = toTree (y x) : toForest (ys x)
    where
      y :: (a .: as) -> a
      y _ = undefined
      ys :: (a .: as) -> as
      ys _ = undefined

instance ToForest VoidX where
  toForest _ = []


-- `a` should never have reached VoidX
  -- ( R
  -- , X (,) .: ((X Int .: VoidX) .: ((X (X Y) .: VoidX) .: VoidX))
  -- , VoidX
  -- , X (,) .: ((X Int .: VoidX) .: ((X (,) .: ((X Int .: VoidX) .: ((X (X Y) .: VoidX) .: VoidX))) .: VoidX))
  -- , (X Int .: VoidX) .: ((X (X Y) .: VoidX) .: VoidX)
  -- , X ('S ('S ('S ('S 'Z)))))

-- | Convenient alias
type family RQ (a :: *) (b :: *) :: * where
  RQ a b = X( Req2 (UnfoldX a) (UnfoldX a) (UnfoldX b) (UnfoldX b) 'Z )


data (:&:) (a :: *) (b :: *)
data R
data D
type family Rc (a :: *) where
  Rc (R, ar, (X XY   ), br, (X XY   ), X d) = X 'True
  Rc (R, ar, (X XY   ), br, (b      ), X d) = (D, ar, ar, br, b, X     d)
  Rc (R, ar, (a      ), br, (X XY   ), X d) = (D, br, br, ar, a, X     d)
  Rc (R, ar, (a .: as), br, (b .: bs), X d) = (R, ar, a , br, b, X ('S d)) :&: (R, ar, as, br, bs, X ('S d))
  Rc (R, ar, (VoidX  ), br, (b      ), X d) = X (VoidX == b)
  Rc (R, ar, (a      ), br, (b      ), X d) = X (a     == b)

  Rc (D, ar, (X XY   ), br, (X XY   ), X ('S d)) = X 'True
  Rc (D, ar, (X XY   ), br, (b      ), X ('S d)) = (D, ar, ar, br, b , X ('S d))
  Rc (D, ar, (a      ), br, (X XY   ), X ('S d)) = (D, ar, a , br, br, X (   d))
  Rc (D, ar, (a .: as), br, (b .: bs), X ('S d)) = (D, ar, a , br, b , X ('S d)) :&: (D, ar, as, br, bs, X ('S d))
  Rc (D, ar, (VoidX  ), br, (b      ), X ('S d)) = X (VoidX == b)
  Rc (D, ar, (a      ), br, (b      ), X ('S d)) = X (a     == b)
  Rc (D, ar, (a      ), br, (b      ), X (  'Z)) = X 'False
  Rc (a :&: b) = Rc a :&: Rc b
  Rc a = a





type family Req2 (ar :: *) (a :: *) (br :: *) (b :: *) (d :: Nat) :: Bool where
  Req2 ar (X XY   ) br (X XY   ) d = 'True
  Req2 ar (X XY   ) br (b      ) d = Deq2 ar ar br b d
  Req2 ar (a      ) br (X XY   ) d = Deq2 br br ar a d
  Req2 ar (a .: as) br (b .: bs) d = Req2 ar a br b ('S d) :&& Req2 ar as br bs ('S d)
  Req2 ar (VoidX  ) br (b      ) d = VoidX == b
  Req2 ar (a      ) br (b      ) d = a     == b

type family Deq2 (ar :: *) (a :: *) (br :: *) (b :: *) (d :: Nat) :: Bool where
  Deq2 ar (X XY   ) br (X XY   ) ('S d) = 'True
  Deq2 ar (X XY   ) br (b      ) ('S d) = Deq2 ar ar br b  ('S d)
  Deq2 ar (a      ) br (X XY   ) ('S d) = Deq2 ar a  br br     d
  Deq2 ar (a .: as) br (b .: bs) ('S d) = Deq2 ar a  br b  ('S d) :&& Deq2 ar as br bs ('S d)
  Deq2 ar (VoidX  ) br (b      ) ('S d) = VoidX == b
  Deq2 ar (a      ) br (b      ) ('S d) = a     == b
  Deq2 ar (a      ) br (b      )    'Z  = 'False

-- undefined :: UnfoldX (Recurse 'Locked (Either Int (IO Bool, XY)))
--   :: X (Recurse 'Locked)
--      .: ((X Either
--           .: ((X Int .: VoidX)
--               .: ((X (,)
--                    .: ((X IO .: ((X Bool .: VoidX) .: VoidX))
--                        .: ((X (X Y) .: VoidX) .: VoidX)))
--                   .: VoidX)))
--          .: VoidX)


-----------------------------------------------------------------------------------

-- | Recurse, checking equality until `XY` is reached.
-- Then, pass the lower depth to the second stage.
type family Req (ar :: *) (a :: *) (br :: *) (b :: *) (d :: Nat) :: Bool

type family ReqExpand (ar :: *) (br :: *) (b :: *) (d :: Nat) where
  ReqExpand ar br XY d = 'True -- Is this really always True? Yup, if a == b, then True.
  ReqExpand ar br b  d = Deq ar ar br b d
type instance Req ar XY br b d = ReqExpand br b XY d

-----------------------------------------------------------------------------------

type family Deq (ar :: *) (a :: *) (br :: *) (b :: *) (d :: Nat) :: Bool

type instance Deq ar a br b 'Z = a == b

type family DeqExpand (ar :: *) (br :: *) (b :: *) (d :: Nat) where
  DeqExpand ar br XY d = 'True
  DeqExpand ar br b  d = Deq ar ar br b d
type instance Deq ar XY br b ('S d) = DeqExpand ar br b d


-- Req ar (Int, XY) br (Int, (Int, XY)) Z
--   Req ar Int br Int (S Z) = True
--   Req ar XY  br (Int, XY) (S Z)
--     Deq ar XY br (Int, XY) (S Z)
--     Deq ar (Int, XY) br (Int, XY) (S Z)
--       Deq ar Int br Int (S Z) = True
--       Deq ar XY  br Int (S Z) = True

-- Req ar (Int, (Int, XY)) br (Int, (Int, (Int, XY))) Z -<      -- ReqTuple2
--   Req ar Int br Int (S Z) = True                            -- DeqAtom
--   Req ar (Int, XY) br (Int, (Int, XY)) (S Z) -<              -- ReqTuple2
--     Req ar Int br Int (S S Z) = True                        -- ReqInt
--     Req ar XY  br (Int, XY) (S S Z) >-                       -- ReqExpand
--     Deq ar XY br (Int, XY) (S S Z) >-                        -- DeqXY
--     Deq ar (Int, (Int, XY)) br (Int, XY) (S S Z) -<          -- DeqTuple2
--       Deq ar Int br Int (S S Z) = True                      -- DeqInt
--       Deq ar (Int, XY) br XY (S S Z) >-                      -- DeqTuple2 -> DeqExpand
--       Deq ar (Int, XY) br (Int, (Int, (Int, XY))) (S Z) -<   -- DeqTuple2
--         Deq ar Int br Int (S Z) = True                      -- DeqInt
--         Deq ar XY  br (Int, (Int, XY)) (S Z) >-              -- DeqExpand
--         Deq ar (Int, (Int, XY)) br (Int, (Int, XY)) (S Z) -< -- DeqTuple2
--           Deq ar Int br Int (S Z) = True                    -- DeqInt
--           Deq ar (Int, XY) br (Int, XY) (S Z) -<             -- DeqTuple2
--             Deq ar Int br Int (S Z) = True                  -- DeqInt
--             Deq ar XY  br XY  (S Z) = True                  -- DeqExpand


-- | If @depth a, depth b@ are coprime and @a `:~:` b@,
-- equality will halt before `Z` is reached.
--
-- This also means that d = 'S d' for all other instances,
-- to prevent overlap.

-- traverse with equality until one xy is reached
--   that one becomes x
--   current depth is depthx
-- traverse with equality until y's xy is reached
--   decrement depthx
--   repeat until depthx == Z
-- if still all eq, x == y

-- Complexity seems to be branching_factor^(depth x * depth y)
-- Assuming pair branches and depth of 8, we get ~256 steps.

-- I did some math and found that traversing (x*y) steps is eventually arbitrarily slower
-- than traversing lcm x y steps, since gcd x y ~~> inf and lcm x y/(x*y) == 1/gcd x y.
-- However, even with billions of terms, it's still only about 25% slower, which I believe
-- is canceled out by the overhead of computing the lcm on the type level.
--
-- Also, if 1) we suppose that the majorities of equalities will be false, and 2) that
-- this will be replaced by unsafeCoerce after the type-checker finishes, there should be
-- little issue with the additional overhead.

