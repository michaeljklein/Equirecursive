module Data.Equirecursive.Tree where

import Data.Equirecursive
import Data.Equirecursive.Class
import Control.Lens
import Control.Monad.Zip
import Control.Monad
import Data.Bitraversable
import Data.Bifunctor
import Data.Tree
import Data.Tuple (swap)
import Control.Comonad

-- this :: Lens s t a b
-- next :: Lens s t a b
-- _0 = this
-- _+ = next
--
-- Basically, I need to be able to map like :: (BTree a -> BTree b) -> (a -> b) -> BTree a -> BTree b
-- next :: (E a -> E b) -> (a -> b) -> E a -> E b
-- newtype Next f a = Next { getNext :: f a }
-- instance Functor (Next BTree) where
--   fmap f x =

-- | Equirecursive tree
newtype ETree a = ETree { getETree :: RecurseL ([XY], a) }

-- | Equirecursive forest
type EForest a = [ETree a]


instance Pull (ETree a) (ETree b) (EForest a, a) (EForest b, b)


instance Functor ETree where
  fmap f = pull %~ ((_1 %~ map (fmap f)) . (_2 %~ f))


-- | Uses a zipping @(`<*>`)@
instance Applicative ETree where
  pure x = (pure (pure x), x) ^. push
  (<*>) = mzipWith ($)


instance Monad ETree where
  return = pure

  xs >>= f = (xf ++ map (>>= f) (xs ^. pull ^. _1), x) ^. push
    where
      (xf, x) = f (xs ^. pull ^. _2) ^. pull


-- | `duplicate` replaces each label with the subtree
-- rooted at that label.
instance Comonad ETree where
  extract = snd . (^. pull)

  duplicate x = pull %~ ((_1 %~ fmap duplicate) . (_2 %~ const x)) $ x


instance MonadZip ETree where
  mzip x y = let ((zf, z), (wf, w)) = bimap (^. pull) (^. pull) (x, y) in (zipWith mzip zf wf, (z, w)) ^. push


instance Foldable ETree where
  foldMap f xs = f y `mappend` foldMap (foldMap f) ys
    where
      (ys, y) = xs ^. pull

-- | Thank you, `Bitraversable`!
instance Traversable ETree where
  sequenceA :: Applicative f => ETree (f a) -> f (ETree a)
  sequenceA = pull (bisequenceA . first (sequenceA . fmap sequenceA))

-- | Convert to a `Tree`
toTree :: ETree a -> Tree a
toTree = uncurry Node . swap . first toForest . (^. pull)

-- | Convert to a `Forest`
toForest :: EForest a -> Forest a
toForest = fmap toTree

