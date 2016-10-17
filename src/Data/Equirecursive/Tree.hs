module Data.Equirecursive.Tree where

import Data.Equirecursive
import Data.Equirecursive.Class
import Control.Lens
import Control.Monad.Zip
import Control.Monad

-- this :: Lens s t a b
-- next :: Lens s t a b
-- _0 = this
-- _+ = next
--
-- Basically, I need to be able to map like :: (BTree a -> BTree b) -> (a -> b) -> BTree a -> BTree b
newtype Next f a = Next { getNext :: f a }
-- instance Functor (Next BTree) where
--   fmap f x =


newtype BTree a = BTree { getBTree :: RecurseL ([XY], a) }



instance Pull (BTree a) (BTree b) (BForest a, a) (BForest a, b)


instance Functor BTree where
  fmap f = pull %~ ((_1 %~ fmap f) . (_2 %~ fmap f) . (_3 %~ f))

instance Applicative BTree where
  pure x = (pure x, pure x, x) ^. push

  (<*>) :: BTree (a -> b) -> BTree a -> BTree b
  (<*>) = mzipWith ($)

instance Monad BTree where
  return = pure

  (>>=) :: BTree a -> (a -> BTree b) -> BTree b
  xs >>= f = join (fmap f xs)
    -- where
      -- join :: BTree (BTree a) -> BTree a
      -- join = pull %~ ((_1 %~ join) . (_2 %~ join) . (_3 %~ join))

instance MonadZip BTree where
  mzip :: BTree a -> BTree b -> BTree (a, b)
  mzip x y = let ((z1, z2, z), (w1, w2, w)) = bimap (^. pull) (^. pull) (x, y) in (z1 `mzip` w1, z2 `mzip` w2, (z, w)) ^. push

instance Foldable BTree where
  foldr :: (a -> b -> b) -> b -> BTree a -> b
  foldr = undefined

instance Traversable BTree where
  sequenceA :: Applicative f => BTree (f a) -> f (BTree a)
  sequenceA = undefined

