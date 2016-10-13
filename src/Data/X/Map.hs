{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}

module Data.X.Map where


import Data.Kind
import Data.Type.Equality
import Data.X
import Data.X.Folding
import GHC.Exts (Coercible) -- , SpecConstrAnnotation)
import Unsafe.Coerce
import Data.Lifted

import Data.Tree
import Data.Maybe
import Control.Applicative

-- data Tree a = Node { rootLabel :: a, subForest :: (Forest a) }

findPrefixTree :: Eq a => Tree a -> Tree a -> [Tree a]
findPrefixTree x@(Node lx _) y@(Node ly fy) | lx == ly   = catMaybes $ getPrefixTree x y : fmap return (findPrefixForest x fy)
                                            | otherwise = findPrefixForest x fy

findPrefixForest :: Eq a => Tree a -> Forest a -> [Tree a]
findPrefixForest x fy = fy >>= findPrefixTree x


getPrefixTree :: Eq a => Tree a -> Tree a -> Maybe (Tree a)
getPrefixTree (Node lx fx) (Node ly fy) | lx == ly   = Node lx <$> getPrefixForest fx fy
                                        | otherwise = Nothing

getPrefixForest :: Eq a => Forest a -> Forest a -> Maybe (Forest a)
getPrefixForest (x:fx) (y:fy) = liftA2 (:) (getPrefixTree x y) (getPrefixForest fx fy)
getPrefixForest    []     fy  = return fy
getPrefixForest    _      []  = Nothing

ta :: Tree Int
ta = Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]

tb :: Tree Int
tb = Node 1 [Node 2 [Node 1 [Node 2 [Node 7 []], Node 3 [Node 4 [Node 9 [], Node 8 []], Node 5 []]], Node 5 []], Node 3 [Node 4 []]]

-- | Example:
-- @X (TestFind Maybe (Either Int) Bool) :: X 'True@
type family TestFind (c :: k0 -> k1) (d :: k1 -> k2) (e :: k0) :: Bool where
  TestFind c d e = ElemX (X (c e)) (Find c (d (c e)))



type family Find (a :: k) (b :: k1) :: * where
  Find a b = FindX (UnfoldX a) (UnfoldX b)

type family SingletonX (a :: *) :: * where
  SingletonX (X a)          = a
  SingletonX    a           = XV

type family Find1 (a :: k) (b :: k1) :: * where
  Find1 a b = SingletonX (Find a b)

-- | The type signature says it all.
-- Its identities are `VoidX` and `XV` and otherwise it's
-- equivalent to `(.:)`
type family (?:) (a :: *) (b :: *) :: * where
  (?:) VoidX b     = b
  (?:) a     VoidX = a
  (?:) XV    b     = b
  (?:) a     XV    = XV
  (?:) a     b     = a .: b

type family FindX (a :: *) (b :: *) :: * where
  FindX (X a .: as) (X a .: VoidX) = FoldX (GetX (X a .: as) (X a .: VoidX)) ?: VoidX
  FindX (X a .: as) (X a .: bs   ) = FoldX (GetX (X a .: as) (X a .: bs)) ?: FindX (X a .: as) bs
  FindX (X a .: as) (X b .: VoidX) = VoidX
  FindX (X a .: as) (X b .: bs   ) = FindX (X a .: as) bs
  FindX (X a .: as) (  b .: VoidX) = FindX (X a .: as) b
  FindX (X a .: as) (  b .: bs   ) = FindX (X a .: as) b ?: FindX (X a .: as) bs ?: VoidX

type family GetX (a :: *) (b :: *) :: * where
  GetX (X a .: as) (X a .: bs) = X a ?: GetX as bs
  GetX (X a .: as) (X b .: bs) = XV
  GetX (  a .: as) (  b .: bs) = GetX a b ?: GetX as bs
  GetX (    VoidX) (       bs) = bs
  GetX (       as) (    VoidX) = XV






-- | Check whether @b@ is constructed from @a@ (on the obvious type level).
-- E.g.
-- @
-- newtype A a = A { getA :: a   }
-- newtype B   = B { getB :: Int }
-- `Elem` `Int` (A Int) ~ '`True`
-- `Elem` `Int`  B      ~ '`False`
-- @
type family Elem  (a :: k0) (b :: k1) :: Bool where
  Elem a b = ElemX (UnfoldX a) (UnfoldX b)

-- | `X`-level `Elem`
type family ElemX (a :: * ) (b :: * ) :: Bool where
  ElemX a (  a         ) = 'True
  ElemX a (X b .: VoidX) = 'False
  ElemX a (X b .: etc  ) =                ElemX a etc
  ElemX a (  b .: VoidX) =  ElemX a b
  ElemX a (  b .: etc  ) =  ElemX a b :|| ElemX a etc
  ElemX a (  b         ) = 'False

-- | Replace all instances of @from@ with @to@ in @a@.
-- See `Elem` for when @from@ is recognized in @a@.
type family MapT  (from :: k0) (to :: k0) (a :: k) :: k where
  MapT from to a = UnX (FoldX (MapTX (UnfoldX from) (UnfoldX to) (UnfoldX a)))

-- | `X`-level `MapT`
type family MapTX (from :: * ) (to :: * ) (a :: k) = (b :: k) where
  MapTX from to (  from      ) =                               to
  MapTX from to (X a .: VoidX) =             X a .: VoidX
  MapTX from to (X a .: etc  ) =             X a .: MapTX from to etc
  MapTX from to (  a .: VoidX) = MapTX from to a .: VoidX
  MapTX from to (  a .: etc  ) = MapTX from to a .: MapTX from to etc
  MapTX from to (  a         ) =               a


-- type family GetC (con :: k -> k1) (a :: k2) :: * where
--   GetC con a = FoldX (GetX (UnfoldX con) (UnfoldX a))


-- undefined :: UnfoldX (Either Int) ∷ X Either .: ((X Int .: VoidX) .: VoidX)

-- | Think of it as `XV` is on fire, and this function
-- lets it burn everything its connected to.
type family Burn (a :: *) :: * where
  Burn a = BurnIf a (ElemX XV (UnfoldX a))

-- | Burn for already unfolded @a@'s
type family BurnX (a :: *) :: * where
  BurnX a = BurnIf a (ElemX XV a)

-- | Internal
type family BurnIf (a :: *) (b :: Bool) :: * where
  BurnIf a 'True  = XV
  BurnIf a 'False = a

-- what happens when we're recursing and have something like this:
-- GetX (X (,) .: (X Int .: VoidX) .: VoidX) (X (,) .: (X Bool .: VoidX) .: VoidX)
-- Well, this is a simpler case: It recurses into
--   X (,) == X (,)
--   GetX ((X Int .: VoidX) .: VoidX) ((X Bool .: VoidX) .: VoidX)
--   GetX (X Int .: VoidX) (X Bool .: VoidX)
--   XV
-- The Burning takes care of it. However, what about
--   GetX (X con .: VoidX) (X a .: etc)?
-- If this was recursed into an equality check,
-- it should fail, but it should still
-- do the
--    GetX (X con .: VoidX) etc
-- recursion. For trees, we have treeA and treeB. We want to find all copies of
-- (treeA + subtrees) in treeB.

-- So here's what we do:
--   We recurse on treeB until we find the head of treeA.
--   Then, we split into two tasks:
--     1. Try to verify that the rest of treeB is here, returning it with subtrees if so
--     2. Continue trying to find other instances of the head of treeA

-- E.g.

-- findPrefixTree :: Tree a -> Tree a -> [Tree a]
-- findPrefixTree pre t | heads match = conatMaybes $ getPrefixTree pre t : findPrefixTree (tail pre) (tail t)
--                      | otherwise  = concatMap (findPrefixTree pre) (tails t)

-- getPrefixTree :: Tree a -> Tree a -> Maybe (Tree a)
-- getPrefixTree pre t | pre == empty     = return t
--                     | heads match = liftM2 (:) (return head) $ getPrefixTree (tail pre) (tail t)
--                     | otherwise   = Nothing

-- treeA

--    1
--   / \
--  2   3
--     / \
--    4   5

-- treeB

--         1
--        / \
--       2   3
--      / \   \
--     1   5   4
--    / \
--   2   3
--  /   / \
-- 7   4   5
--    / \
--   9   8



-- type family GetX (con :: *) (a :: *) :: * where
-- --GetX (X con .: VoidX) (X con .: VoidX) = X con .: VoidX
--   GetX (X con .: VoidX) (X con .: etc  ) = X con .: etc
--   GetX (X con .: VoidX) (X a   .: VoidX) = XV
--   GetX (X con .: VoidX) (X a   .: etc  ) = GetX (X con .: VoidX) etc
--   GetX (X con .: VoidX) (  a   .: VoidX) = GetX (X con .: VoidX) a
--   GetX (X con .: VoidX) (  a   .: etc  ) = XRec2
--   GetX (X con .: cons ) (X con .: VoidX) = XV
--   GetX (X con .: cons ) (X con .: etc  ) = XRec & Burn
--   GetX (X con .: cons ) (X a   .: VoidX) = XV
--   GetX (X con .: cons ) (X a   .: etc  ) = GetX (X con .: cons ) etc
--   GetX (X con .: cons ) (  a   .: VoidX) = GetX (X con .: cons ) a
--   GetX (X con .: cons ) (  a   .: etc  ) = XRec2 & Burn
--   GetX (  con .: VoidX) (X a   .: VoidX) = XV
--   GetX (  con .: VoidX) (X a   .: etc  ) = XV
--   GetX (  con .: VoidX) (  a   .: VoidX) = GetX (  con .: VoidX) a
--   GetX (  con .: VoidX) (  a   .: etc  ) = XRec2 & Burn
--   GetX (  con .: cons ) (X a   .: VoidX) = XV
--   GetX (  con .: cons ) (X a   .: etc  ) = GetX (  con .: cons ) etc
--   GetX (  con .: cons ) (  a   .: VoidX) = GetX a?
--   GetX (  con .: cons ) (  a   .: etc  ) = X


  -- simpleCon = X con
  -- complexCon = X con .: a .: b .: VoidX

-- | Equivalent datatypes can be coerced between safely (assuming the types are not ?nominal?)
mapTCoerce  :: Coercible from to => a -> MapT from to a
mapTCoerce  = unsafeCoerce

-- | Inverse of `mapTCoerce`
mapTCoerce' :: Coercible from to => MapT from to a -> a
mapTCoerce' = unsafeCoerce

-- | @`MapT` from to@ is the inverse of @`MapT` to from@
-- TODO: only if not (from `elem` a)
mapTInverse :: forall from to a. MapT from to (MapT to from a) :~: a
mapTInverse = unsafeCoerce Refl

-- | @`MapT` from to a `==` a@ on types that do not contain @from@.
-- (See `Elem` for when subtypes can be recognized.)
mapTElem :: forall from to a. (MapT from to a == a) :~: (Not (Elem from a) :|| (from == to))
mapTElem = unsafeCoerce Refl

-- | `MapT` replaces all instances of @from@ in @a@ with @to@, so the only way that
-- @`Elem` from (`MapT` from to a) ~ 'True` is when @from `==` to@.
mapTElem2 :: forall from to a. Elem from (MapT from to a) :~: (from == to)
mapTElem2 = unsafeCoerce Refl

-- | `MapT` is idempotent
mapTIdempotent :: forall from to a. MapT from to (MapT from to a) :~: MapT from to a
mapTIdempotent = unsafeCoerce Refl



