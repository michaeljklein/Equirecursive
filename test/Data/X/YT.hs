
-- | Rename to Control.Monad.Trans.YT
module Data.X.YT where

import Control.Applicative
import Control.Lens
import Control.Lens.Iso
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Profunctor.Unsafe
import Data.X
import Prelude hiding (fail)
import Unsafe.Coerce


newtype YT m a = YT { runYT :: m (Y a) }

yT :: Iso (YT m a) (YT m b) (m a) (m b)
yT = dimap unsafeCoerce (fmap unsafeCoerce)

liftYT2 :: (m a -> m b -> m c) -> YT m a -> YT m b -> YT m c
liftYT2 f x y = ((x ^. yT) `f` (y ^. yT)) ^. from yT

instance Functor m => Functor (YT m) where
  fmap f = yT %~ fmap f

instance Applicative m => Applicative (YT m) where
  pure x = pure x ^. from yT

  (<*>) :: YT m (a -> b) -> YT m a -> YT m b
  (<*>) = liftYT2 (<*>)

instance Monad m => Monad (YT m) where
  return = pure

  (>>=) :: YT m a -> (a -> YT m b) -> YT m b
  x >>= f = ((x ^. yT) >>= ((^. yT) <$> f)) ^. from yT

instance MonadTrans YT where
  lift = (^. from yT)

instance MonadFix m => MonadFix (YT m) where
  mfix :: (a -> YT m a) -> YT m a
  mfix = (^. from yT) . mfix . fmap (^. yT)

instance MonadFail m => MonadFail (YT m) where
  fail = (^. from yT) . fail

instance MonadIO m => MonadIO (YT m) where
  liftIO :: IO a -> YT m a
  liftIO = (^. from yT) . liftIO

instance Alternative m => Alternative (YT m) where
  empty :: YT m a
  empty = empty ^. from yT

  (<|>) :: YT m a -> YT m a -> YT m a
  (<|>) = liftYT2 (<|>)

  some :: YT m a -> YT m [a]
  some = yT %~ some

  many :: YT m a -> YT m [a]
  many = yT %~ many

instance MonadPlus m => MonadPlus (YT m) where
  mzero = mzero ^. from yT

  mplus = liftYT2 mplus


