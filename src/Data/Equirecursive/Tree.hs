module Data.Equirecursive.Tree where

import Data.Equirecursive
import Data.Equirecursive.Class


newtype BTree a = BTree { getBTree :: RecurseL (XY, XY, a) }

instance Pull (BTree a) (BTree a, BTree a, a)

newtype Next f a = Next { getNext :: f a }

-- instance Functor (Next BTree) where
--   fmap f x =

instance Functor BTree where
  fmap f x =

-- instance Functor BTree
-- instance Applicative BTree
-- instance Monad BTree
-- instance Foldable BTree
-- instance Traversible BTree

