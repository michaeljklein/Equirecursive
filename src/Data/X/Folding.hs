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

module Data.X.Folding where

import Data.Kind
import Data.Tree
import Data.Typeable
import Data.X

-- | Recursively unfold a type
type family UnfoldX (a :: Type) :: Type where
  UnfoldX a = UnfoldXL VoidX (X a)

-- | Recursively unfold a type with given argument list
type family UnfoldXL (l :: Type) (a :: Type) :: Type where
  UnfoldXL l (X (c a)) = UnfoldXL (UnfoldX a .: l) (X c) .|| X c .: (UnfoldX a .: l)
  UnfoldXL l (X  c   ) =                                     X c .:               l


-- | Recursively fold a (X type :. type list)
-- Should have:
-- @`FoldX` (`UnfoldX` a) == `X` a@
type family FoldX (a :: Type) :: Type where
  FoldX (X c .:  VoidX  ) =        X c
  FoldX (X c .: (a .: b)) = FoldX (X c .$ FoldX a .: b)

-- | Show an unfolded type in tree form
showx :: ToTree a => a -> String
showx = drawTree . toTree

-- | Print a type unfolded and in tree form
printu :: ToTree (UnfoldX a) => a -> IO ()
printu = putStrLn . showx . (undefined :: a -> UnfoldX a)

-- | See `toTree`
class ToTree (a :: Type) where
  -- | Convert an unfolded type into a `Tree` of `String`s
  toTree   :: a -> Tree String

-- | See `ToTree`
class ToForest (a :: Type) where
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


