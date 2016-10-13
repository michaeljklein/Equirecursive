{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.X.Folding where

import Data.Kind
import Data.Tree
import Data.Typeable
import Data.X
import GHC.TypeLits( ErrorMessage(..), TypeError )

-- Idea: Abstract a function, or module. Abstracting a single function just is
-- func_example :: t -> $(abstract 'func_example) = func const1 .. constn :: (typeOf const1) -> .. -> (typeOf constn) -> t
-- Abstracting multiple functions is harder. Consider:
-- bindMaybe_example1 (Just f) x = Just (f x)
-- bindMaybe_example2 Nothing  _ = Nothing
-- bindMaybe = (>>=)
-- $(abstractN 'bindMaybe_example1 'bindMaybe_example2)
--
-- mySum_example1 x y   = x + y
-- mySum_example2 x y z = x + y + z  -- or even mySum_example2 = 1 + 2 + 3?
-- mySum                = foldl1 (+)
-- $(abstractN 'mySum_example1 'mySum_example2)
--
-- Need to take the source-lcm of the examples somehow, though obviously only a few types of abstractions could be supported.


data Unfold a where
  Unfold :: a -> Unfold (UnfoldX a)


-- | Recursively unfold a type
type family UnfoldX (a :: k) :: Type where
  UnfoldX a = UnfoldXL VoidX (X a)

-- | Recursively unfold a type with given argument list
type family UnfoldXL (l :: Type) (a :: Type) :: Type where
  UnfoldXL l (X (X Y)) =                 X (X Y) .: l
  UnfoldXL l (X (c a)) = UnfoldXL (UnfoldX a .: l) (X c) .|| (X c .: (UnfoldX a .: l))
  UnfoldXL l (X  c   ) =                 X c .: l


-- | Recursively fold a (X type :. type list)
-- Should have:
-- @`FoldX` (`UnfoldX` a) == `X` a@
type family FoldX (a :: Type) = (b :: Type) where
  FoldX (X (c :: k) .:  VoidX  ) =        X (c :: k)
  FoldX (X (c :: k) .: (a .: b)) = FoldX (X c .$ FoldX a .: b)

-- | This is like fromJust
type family FoldXType (a :: Type) :: Type where
  FoldXType (X (c :: Type) .:  VoidX  ) =              c
  FoldXType (X (c :: k   ) .:  VoidX  ) = TypeError ('Text "FoldXType: Type " ':<>: 'ShowType c ':<>: 'Text "does not have kind *.")
  FoldXType (X (c        ) .: (a .: b)) = FoldXType (X (c (FoldXType a)) .: b)


-- | Show an unfolded type in tree form
showToTree :: ToTree a => a -> String
showToTree = drawTree . toTree

-- | Print a type unfolded and in tree form
printUnfolded :: ToTree (UnfoldX a) => a -> IO ()
printUnfolded = putStrLn . showToTree . (undefined :: a -> UnfoldX a)

-- | See `toTree`
class ToTree (a :: Type) where
  -- | Convert an unfolded type into a `Tree` of `String`s
  toTree   :: a -> Tree String

-- | See `ToTree`
class ToForest (a :: Type) where
  -- | See `toTree`
  toForest :: a -> Forest String

-- | @`X` a@ forms an empty node with label @`typeOf` (`X` a)@
instance Typeable (X a) => ToTree (X a) where
  toTree x = Node (show . typeOf $ x) []

-- | This forms a node with label @`typeOf` (`X` a)@ and
-- subforest @`toForest` bs@
instance (Typeable (X a), ToForest bs) => ToTree (X a .: bs) where
  toTree x = Node (label x) (toForest (rm x))
    where
      hd :: (X a .: bs) -> X a
      hd _ = undefined
      label = drop 2 . show . typeOf . hd
      rm :: (X a .: bs) -> bs
      rm _ = undefined

-- | This forms a forest with head @`toTree` a@ and
-- tail @`toForest` as@
instance (ToTree a, ToForest as) => ToForest (a .: as) where
  toForest x = toTree (y x) : toForest (ys x)
    where
      y :: (a .: as) -> a
      y _ = undefined
      ys :: (a .: as) -> as
      ys _ = undefined

-- | Void forests are empty.
instance ToForest VoidX where
  toForest _ = []

-- | Check whether a type is atomic
type family IsAtom (a :: k) :: Bool where
  IsAtom (c a) = 'False
  IsAtom    a  = 'True




