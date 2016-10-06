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
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE InstanceSigs #-}

{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Rename to Data.Recurse.Recursing
module Data.Recurse.Recursing where

import Data.Recurse
-- import Data.X
-- import Data.X.Map
-- import Control.Lens.Setter
import Prelude hiding (id, (.))
import Data.Kind
import Data.X
import Data.X.Map hiding (XMap)
import Unsafe.Coerce
import Control.Category (Category(..))
import Data.Function (fix)
import Control.Lens.Setter ((%~))
import Data.Void

-- class Recursing (a :: *) where
--   type RecursingBuilder a t :: *
  -- rec :: (forall t. RecursingBuilder a t) -> Recurse 'Locked a

type Y = (X :: * -> *)

type XY = X Y

type RVoid = Recurse 'Locked Void


lockA :: forall a b. (a -> b) -> (RecurseU a -> XY)
lockA _ (RecurseUnlocked x) = X (unsafeCoerce x)

unlockV :: XY -> RVoid
unlockV (X x) = RecurseLocked (unsafeCoerce x)

-- rec ::
--     (t -> s) -> RecurseL (YMapF s Y 'Unlocked t)
-- rec = RecurseLocked . fix . (. (xmapn %~ unlockV)) . ((.) =<< (ymapn %~) . lockA)

class ( XMapC (YMapF s Y 'Unlocked t) Y 'Locked Void
      , XMapF (YMapF s Y 'Unlocked t) Y 'Locked Void ~ t
      , XMapN (YMapF s Y 'Unlocked t)
      , XMapN s
      , YMapC s Y 'Unlocked t
      ) => Recursing t s where
        rec :: (t -> s) -> RecurseL (YMapF s Y 'Unlocked t)
        rec = RecurseLocked . fix . (. (xmapn %~ unlockV)) . ((.) =<< (ymapn %~) . lockA)

instance ( XMapC (YMapF s Y 'Unlocked t) Y 'Locked Void
         , XMapF (YMapF s Y 'Unlocked t) Y 'Locked Void ~ t
         , XMapN (YMapF s Y 'Unlocked t)
         , XMapN s
         , YMapC s Y 'Unlocked t
         ) => Recursing t s where
        rec :: (t -> s) -> RecurseL (YMapF s Y 'Unlocked t)
        rec = RecurseLocked . fix . (. (xmapn %~ unlockV)) . ((.) =<< (ymapn %~) . lockA)



-- lockAs :: forall a b k. (XMapN b, YMapC b (X :: k -> *) 'Unlocked a) =>
--                 (a -> b) -> a -> YMapF b (X :: k -> *) 'Unlocked a
-- lockAs f = ((ymapn :: Setter b (YMapF b (X :: k -> *) 'Unlocked a) (Recurse 'Unlocked a) (X (X :: k -> *))) %~ lockA' f) . f

-- biLockV :: forall a b k. (XMapN a, XMapN b, XMapC a (X :: k -> *) 'Locked Void, YMapC b (X :: k -> *) 'Locked Void) =>
--   (XMapF a (X :: k -> *) 'Locked Void -> b) -> a -> YMapF b (X :: k -> *) 'Locked Void
-- biLockV f = ((ymapn :: Setter b (YMapF b (X :: k -> *) 'Locked Void) (Recurse 'Locked Void) (X (X :: k -> *))) %~ (lockV' :: Recurse 'Locked Void -> X (X :: k -> *))) . f . ((xmapn :: Setter a (XMapF a (X :: k -> *) 'Locked Void) (X (X :: k -> *)) (Recurse 'Locked Void)) %~ (unlockV' :: X (X :: k -> *) -> Recurse 'Locked Void))





-- type T0   k = X (X :: k -> *) -> Recurse 'Locked Void
-- type T1 a k = (XMapN a, XMapC a (X :: k -> *) 'Locked Void) => (X (X :: k -> *) -> Identity (Recurse 'Locked Void)) -> a -> Identity (XMapF a (X :: k -> *) 'Locked Void)
-- type T2 a k = (XMapN a, XMapC a (X :: k -> *) 'Locked Void) => a -> XMapF a (X :: k -> *) 'Locked Void

-- f1 :: forall a k. T2 a k
-- f1 = (runIdentity . (xmapn :: T1 a k) (Identity . (unlockV' :: T0 k)))

-- type T22 a b k = (XMapN a, XMapC a (X :: k -> *) 'Locked Void) => (XMapF a (X :: k -> *) 'Locked Void -> b) -> a -> b

-- f1' :: forall a b k. T22 a b k
-- f1' = (. (runIdentity . (xmapn :: T1 a k) (Identity . (unlockV' :: T0 k))))

-- type T3   k = Recurse 'Locked Void -> X (X :: k -> *)
-- type T4 a k = (XMapN a, YMapC a (X :: k -> *) 'Locked Void) => a -> YMapF a (X :: k -> *) 'Locked Void

-- f2 :: forall a k. T4 a k
-- f2 = runIdentity . ymapn (Identity . (lockV' :: T3 k))

-- type T5 a b k = (a -> b) -> (Recurse 'Unlocked a -> X (X :: k -> *))
-- type T6 a b k = (XMapN b, YMapC b (X :: k -> *) 'Unlocked a) => (a -> b) -> a -> YMapF b (X :: k -> *) 'Unlocked a

-- f3 :: forall a b k. T6 a b k
-- f3 r = (over ymapn ((lockA' :: T5 a s k) r)) . r

-- type T7 a b k = (XMapN a, XMapN b, XMapC a (X :: k -> *) 'Locked Void, YMapC b (X :: k -> *) 'Locked Void) => (XMapF a (X :: k -> *) 'Locked Void -> b) -> a -> YMapF b (X :: k -> *) 'Locked Void

-- f4 :: forall a b k. T7 a b k
-- f4 = ((runIdentity . ymapn (Identity . (lockV' :: T3 k))) .) . (. (runIdentity . (xmapn :: T1 a k) (Identity . (unlockV' :: T0 k))))



-- f5 :: forall b t k. (YMapF (YMapF b (X :: * -> *) 'Locked Void) (X :: * -> *) 'Unlocked t ~ t, XMapN t,
--   XMapN (YMapF b (X :: * -> *) 'Locked Void), XMapN b, YMapC b (X :: * -> *) 'Locked Void,
--   XMapC t (X :: * -> *) 'Locked Void,
--   YMapC (YMapF b (X :: * -> *) 'Locked Void) (X :: * -> *) 'Unlocked t) =>
--   (XMapF t (X :: * -> *) 'Locked Void -> b) -> t

-- f5 = fix . (f3 :: T6 a b Type) . (f4 :: T7 a b Type)


------

-- type T7 a b k = (XMapN a, XMapN b, XMapC a (X :: k -> *) 'Locked Void, YMapC b (X :: k -> *) 'Locked Void) => (XMapF a (X :: k -> *) 'Locked Void -> b) -> a -> YMapF b (X :: k -> *) 'Locked Void

-- f4 :: forall a b k. T7 a b k
-- f4 = ((f2 :: T4 b k) .) . (. (f1 :: T2 a k))


-- fix . f3 . (f2 .) . (. f1)
--   :: (YMapF (YMapF a X 'Locked Void) X 'Unlocked t ~ t, XMapN t,
--       XMapN (YMapF a X 'Locked Void), XMapN a, XMapC t X 'Locked Void,
--       YMapC a X 'Locked Void,
--       YMapC (YMapF a X 'Locked Void) X 'Unlocked t) =>
--      (XMapF t X 'Locked Void -> a) -> t

--  f >>= k = \ r -> k (f r) r

-- xmapn :: forall k0 s (a :: k0) (l :: Locking) b (f :: * -> *).  (XMapN s, Settable f, XMapC s a l b) =>
--      (X a -> f (Recurse l b)) -> s -> f (XMapF s a l b)



-- type T1 k = X (X :: k -> *) -> Recurse 'Locked Void
-- type T2 a k =


-- ff = fix . ((.) =<< (ymapn %~) . lockA') . ((runIdentity.ymapn(Identity. lockV')) .) . (. (runIdentity.xmapn(Identity.(unlockV'::T1 k))))

-- fix . ((.) =<< (ymapn %~) . lockA') . ((ymapn %~ lockV') .) . (. (xmapn %~ (unlockV'::XX k->RecurseL Void)))
--   :: (YMapF (YMapF s X 'Locked Void) X 'Unlocked t ~ t, XMapN t,
--       XMapN (YMapF s X 'Locked Void), XMapN s, XMapC t X 'Locked Void,
--       YMapC s X 'Locked Void,
--       YMapC (YMapF s X 'Locked Void) X 'Unlocked t) =>
--      (XMapF t X 'Locked Void -> s) -> t




-- (.) :: (b -> c) -> (a -> b) -> a -> c

-- (.!) :: (b -> c) -> (a -> b) ->

-- c2 :: (b -> c) -> (b1 -> b) -> (a -> b1)

-- \x y z -> x . y . z :: (b -> c) -> (b1 -> b) -> (a -> b1) -> a -> c


-- (RecurseLocked :: _)
-- . (fix :: _)
-- . (\f -> ((ymapn :: Setter (YMapF b (X :: k -> *) 'Locked Void) (YMapF (YMapF b (X :: k -> *) 'Locked Void) (X :: k -> *) 'Unlocked a) (Recurse 'Unlocked a) (X (X :: k -> *))) %~ lockA' f) . f :: (a -> YMapF b (X :: k -> *) 'Locked Void) -> a -> YMapF (YMapF b (X :: k -> *) 'Locked Void) (X :: k -> *) 'Unlocked a)
-- . (\f -> ((ymapn :: Setter b (YMapF b (X :: k -> *) 'Locked Void) (Recurse 'Locked Void) (X (X :: k -> *))) %~ (lockV' :: Recurse 'Locked Void -> X (X :: k -> *))) . f . ((xmapn :: Setter a (XMapF a (X :: k -> *) 'Locked Void) (X (X :: k -> *)) (Recurse 'Locked Void)) %~ (unlockV' :: X (X :: k -> *) -> Recurse 'Locked Void)))

-- rec :: forall b t k. ((YMapF
--          (YMapF b (X :: k -> *) 'Locked Void)
--          (X :: k -> *)
--          'Unlocked
--          t)
--       ~
--       t,
--       XMapN t, XMapN (YMapF b (X :: k -> *) 'Locked Void), XMapN b,
--       YMapC b (X :: k -> *) 'Locked Void,
--       XMapC t (X :: k -> *) 'Locked Void,
--       YMapC
--         (YMapF b (X :: k -> *) 'Locked Void)
--         (X :: k -> *)
--         'Unlocked
--         t) =>
--      (XMapF t (X :: k -> *) 'Locked Void -> b) -> Recurse 'Locked t


-- rec = RecurseLocked . fix . (\f -> ((ymapn :: Setter (YMapF b (X :: k -> *) 'Locked Void) (YMapF (YMapF b (X :: k -> *) 'Locked Void) (X :: k -> *) 'Unlocked a) (Recurse 'Unlocked a) (X (X :: k -> *))) %~ lockA' f) . f) . (\f -> ((ymapn :: Setter b (YMapF b (X :: k -> *) 'Locked Void) (Recurse 'Locked Void) (X (X :: k -> *))) %~ (lockV' :: Recurse 'Locked Void -> X (X :: k -> *))) . f . ((xmapn :: Setter t (XMapF t (X :: k -> *) 'Locked Void) (X (X :: k -> *)) (Recurse 'Locked Void)) %~ (unlockV' :: X (X :: k -> *) -> Recurse 'Locked Void))) :: forall b t k.

-- lockAs :: forall a b k. (XMapN b, YMapC b (X :: k -> *) 'Unlocked a) =>
--                 (a -> (YMapF b (X :: k -> *) 'Locked Void)) -> a -> YMapF (YMapF b (X :: k -> *) 'Locked Void) (X :: k -> *) 'Unlocked a

-- biLockV :: forall a b k. (XMapN a, XMapN b, XMapC a (X :: k -> *) 'Locked Void, YMapC b (X :: k -> *) 'Locked Void) =>
--   (XMapF a (X :: k -> *) 'Locked Void -> b) -> a -> (YMapF b (X :: k -> *) 'Locked Void)




-- xmapn :: X a -> Rec b
-- ymapn :: Rec b -> X a
-- Rec a -> Rec b
-- X a -> X b
-- (Rec b -> X b) . f . (X a -> Rec a)


-- biLockV :: forall a b k. (XMapN a, XMapN b, YMapC a (X :: k -> *) 'Locked Void, YMapC b (X :: k -> *) 'Locked Void) =>
--   (YMapF a (X :: k -> *) 'Locked Void -> b) -> a -> YMapF b (X :: k -> *) 'Locked Void
--   biLockV f = (lockedV :: b -> YMapF b (X :: k -> *) 'Locked Void) . f . (lockedV :: YMap

-- biLockV :: (XMapN s1, XMapN s, YMapC s1 (X :: * -> *) 'Locked Void, YMapC s (X :: * -> *) 'Locked Void) =>
--     (YMapF s1 (X :: * -> *) 'Locked Void -> s) -> s1 -> YMapF s (X :: * -> *) 'Locked Void
-- biLockV = \f -> (ymapn %~ lockV) . f . (ymapn %~ lockV)

-- biLockV' :: XMapN a, XMapN b, YMapC a (X :: k -> *) 'Locked Void, YMapC b (X :: k -> *) 'Locked Void) =>
--   (YMapF a (X :: k -> *) 'Locked Void -> b) -> a -> YMapF b (X :: k -> *) 'Locked Void
-- biLockV' = \f -> (ymapn :: Setter

-- ymapn :: forall (a :: k0) (l :: Locking) (b :: *). YMapC s a l b => Setter s (YMapF s a l b) (Recurse l b) (X       a  )

-- lockAs :: forall a b. (XMapN b, YMapC b (X :: * -> *) 'Unlocked a) =>
--                 (a -> b) -> a -> YMapF b (X :: * -> *) 'Unlocked a
-- lockAs f = (ymapn %~ lockA f) . f

-- rec :: (YMapF (YMapF s (X :: * -> *) 'Locked Void) (X :: * -> *) 'Unlocked t ~ t, XMapN t,
--   XMapN (YMapF s (X :: * -> *) 'Locked Void), XMapN s, YMapC s (X :: * -> *) 'Locked Void,
--   YMapC t (X :: * -> *) 'Locked Void,
--   YMapC (YMapF s (X :: * -> *) 'Locked Void) (X :: * -> *) 'Unlocked t) =>
--   (YMapF t (X :: * -> *) 'Locked Void -> s) -> Recurse 'Locked t
-- rec = RecurseLocked . fix . lockAs . biLockV

-- biYmapn = biSetter biArrow (ymapn

-- ymapn :: (XMapN s, Settable f, YMapC * s a l b) => (Recurse l b -> f (X * a)) -> s -> f (YMapF * s a l b)

-- lockingV :: (XMapN s1, XMapN s0, Arrow arr, YMapC s1 X 'Locked Void,
--       YMapC (s0 X 'Locked Void) =>
--      arr (YMapF s0 X 'Locked Void) s1
--      -> arr s0 (YMapF s1 X 'Locked Void)
-- lockingV = biSetter biArrow ymapn (ymapn :: Setter s1 (YMapF s1 (a :: * -> *) 'Locked Void) (Recurse 'Locked Void) (X (a :: * -> *))) %~ lockV

-- ff = fix . (\f -> ((ymapn %~ lockA f) .) f) . (biSetter biArrow ymapn ymapn %~ lockV)

 -- (ymapn %~ lockA f)

-- xrec :: (a -> b) -> a -> c

-- -- | `xmap`-like
-- (~%~) :: forall a a1 a2 b b1 b2 b3 c.
--          ( XMap b2 a a2 b3
--          , XMap b  c a1 b1 ) =>
--          (a1 -> b1) -> (a2 -> b3) -> (b2 -> b) -> a -> c
-- (f ~%~ g) ar = (xmap %~ f) . ar . (ymap %~ g)

-- -- | `ymap`-like
-- (%~%) :: forall a a1 a2 b b1 b2 b3 c.
--          ( XMap a b2 a2 b3
--          , XMap c b  a1 b1 ) =>
--          (a1 -> b1) -> (a2 -> b3) -> (b2 -> b) -> a -> c
-- (f %~% g) ar = (ymap %~ f) . ar . (xmap %~ g)




-- class (XMap b4 b2 (RecurseU a1) (RecurseL a1), XMap b3 b1 (RecurseU a) (RecurseL a), XMap b b2 (XX k1) (RecurseL t1), XMap b b1 (XX k) (RecurseL t)) => Recursing a b c where
--   rec :: (b4 -> b3) -> RecurseL b


-- rec f = RecurseLocked . fix $ (lockXX %~% lockXX) . (lock ~%~ lock) $ f

tr4 :: (Recurse 'Locked t, Int) -> (Recurse 'Locked (Recurse 'Locked t, Int), Int)
tr4 = undefined

-- tt = rec tr4

-- xmap %~ lock :: XMap s t (RecurseU a) (Recurse 'Locked a) => s -> t
-- xmap %~ lockXX :: XMap s t (XX k) (Recurse 'Locked t1) => s -> t
-- ymap %~ lockXX :: XMap t t1 (XX k) (Recurse 'Locked t2) => t1 -> t

-- Got locked and unlocked backwards? Unlocked should be the one that's replaced while Locked should be the one that's mapped through.
-- Unlocked has the functor instance while Locked has the functor-like instance.
--
-- The functor-like instance allows one to recurse on the locked one?
-- Recurse 'Locked (XX k, Int) -> (Recurse 'Locked (XX k, Int), Int)

-- let tr4 = undefined :: (Recurse 'Locked t, Int) -> (Recurse 'Locked (Recurse 'Locked t, Int), Int)

-- RecurseLocked . fix . runIdentity $ (ymapArrow2 (undefined :: XX k -> Identity (Recurse 'Locked t)) (undefined :: XX k -> Identity(Recurse 'Locked t))) tr4
--   :: Recurse 'Locked (XX k, Int)


-- instance XMap (XX k) (Recurse 'Locked t) (XX k) (Recurse 'Locked t)
-- instance XMap (Recurse 'Unlocked s) (Recurse 'Unlocked t) a b

-- This means we effectivey have an iso between (XX k) and (Recurse 'Locked t).
-- The second means we can map into (Recurse 'Unlocked)


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- (RecurseLocked . fix . (ymap `over` (undefined :: XX k -> Rec t))) (undefined :: (Int, Rec t) -> (Int, Rec t))
--   :: Recurse 'Locked (Int, XX k)
-- (RecurseLocked . fix . (ymap `over` (undefined :: XX k -> Rec t)))
--   :: XMap a t1 (X X) (Rec t) => (t1 -> t1) -> Recurse 'Locked a


-- rec :
-- RecurseLicked . fix . ymap `over` (undefined :: XX k -> Rec t)

--   RecurseLocked   :: a -> Recurse 'Locked   a
--   RecurseUnlocked :: a -> Recurse 'Unlocked a


-- instance Recursing (Int, XX k) where
--   type RecBuilder (Int, XX k) t = (Int, Rec t) -> (Int, Rec (Int, Rec t))
--   rec r = let t = r t in Rec t

-- return . fix . ymap `over` (undefined :: Rec a -> XX k)

-- Here is the control flow for deriving the types:
-- Rec (a, XX k) ->         Given in class
--   (a, XX k) ->           getRec
--     (a, Rec t)           over xmap (_ :: XX k -> Rec t)
--     (a, (a, XX k)) ->    over xmap (_ :: XX k -> (a, XX k))
--       (a, (a, Rec t))  over xmap (_ :: XX k -> Rec t)
--
-- By doing a type family application, we get: forall t. (a, Rec t) -> (a, (a, Rec t))
--
-- We then have to generically bind a function of this type into an infinite loop
-- and properly coerce to Rec (a, XX k).
--
-- If we coerce away the Rec's, we get (a, t) -> (a, (a, t)), which can be recursed over.
-- However, hmmmmmmm.
--
-- *:     (a, Rec t) -> (a,     (a, Rec t))
-- 1:     (a, Rec t) -> (a, Rec (a, Rec t))
-- 2:     (a, XX  k) -> (a, Rec (a, XX  k))  SKIP, ymap `over` (_ :: Rec t -> _) kills Rec (a, _)
-- 3:     (a, XX  k) -> (a,         XX  k)   ymap `over` (_ :: Rec (a, Rec t) -> XX k)
-- 4:     (a, XX  k)                         fix
-- 5: Rec (a, XX  k)                         return
--
-- Here's where the ymap's come in. I believe that the * case
-- makes it impossible to coerce further. However, I know the
-- second case must be skipped.
--
-- This should do it, all that has to be worked out is the instances, constraints,
-- (->) builder type family(?), and what level of access will be had of Rec (I'm thinking probably functor, maybe applicative)
--
-- NOTE: reconsider the (k -> *) instances for X. It may allow one to kill the X's inside of Rec, if Rec is a functor, which
-- it'll probably have to be.


-- Rec (a, XX k) ->
-- forall t. (a, t) -> (a, (a, t))
--   ||
-- forall t. (a, Rec t) -> (a, (a, Rec t))
--   ||
-- forall t. (a, Rec t) -> (a, Rec (a, t))

-- (a, XX k) -> (a, Rec t)
-- (a, XX k) -> (a, Rec (a, XX k)) -> (a, Rec (a, Rec t))

-- Then, we only need Rec to be a functor, though possibly limited, or even keeping it even more general and
-- giving an iso to Rec's not containing XX. The definitions will have to be modified to account for moving inside
-- Rec, but it should work fine.

-- there are two types of recursion:
-- t -> c t, c t -> c (c t)
-- The first type is effectively trivial, and is more
-- general type-wise than the second, but the second is
-- more general functionality wise.
--
-- One method is to apply to the least common denominator and always
-- require the second. The other method is to attempt to double-up on the instances
-- since there's a type-level iso between them. What about a value-level iso?
--
-- Going from the first to the second is easy, but how do we differentiate between
-- the two? Ahhhhh.... We don't have to!! We have:
--  id = (t -> c t) -> (c t -> c (c t))
-- So we can ignore the first as far as constructors, but we should still simplify the
-- r-types to r'-types.


-- There's really only one way to do this: We need to replace the first argument of
-- the t-types with XX k in the entire t-type.
-- Why only one way? Without depending on the first argument, we can't tell the difference between
-- (1,(2,(1,(2...)))) and (1, (([_,2,1]!!), ...)).
-- The first is static, simple, recursion. the second references previous values.


-- Compare r4 and r4'. The type of r4 tells us that the recursive function
-- references its input (when viewed vs. t4). The type of r4' hides that
-- information, but may be derived with more ease?

-- One clear point is that the non-prime r-types contain more information than is needed once
-- wrapped in Rec.
--
-- How can we convert the t-types into the r'-types??

-- Should this one be an iso?
-- Hmmmm.. Probably not.
-- We do need to 1) know how to get from ti -> ri, but that "might" be as easy as (`asTypeOf` t4 xX)
-- 2) do we need to be able to get back?

-- class Recurse


-- t1 :: forall t. t -> (Integer, t)
-- t1 = \x -> (0, x)

-- t1' :: forall t. (Integer, Rec t) -> (Integer, Rec (Integer, Rec t))
-- t1' = \x -> (0, Rec x)

-- r1 :: Rec (Integer, XX k)
-- r1 = undefined

-- -- | Trivial conversion
-- r1' :: Rec (Integer, XX k)
-- r1' = undefined


-- t2 :: forall t. (Integer, t) -> (Integer, (Integer, t))
-- t2 = \x -> (0, first (+1) x)

-- t2' :: forall t. (Integer, Rec t) -> (Integer, Rec (Integer, Rec t))
-- t2' = \x -> (0, Rec $ first (+1) x)

-- r2 :: Rec (Integer, (Integer, XX k))
-- r2 = undefined

-- r2' :: Rec (Integer, XX k)
-- r2' = undefined


-- -- Not sure about this one
-- t3 :: forall a d. (a -> (Integer, d)) -> Integer -> (Integer, a -> (Integer, d))
-- t3 = \x -> \y -> (y + 1, (first ((+1)$) . x))


-- instance Functor ((,,) a b) where
--   fmap f (x,y,z) = (x,y,f z)

-- t4 :: forall t. (t, t, Integer) -> ((t, t, Integer), (t, t, Integer), Integer)
-- t4 = \x -> ((*2) <$> x, (+1).(*2) <$> x, 0)

-- t4' :: forall t. (Rec t, Rec t, Integer) -> (Rec (Rec t, Rec t, Integer), Rec (Rec t, Rec t, Integer), Integer)
-- t4' = undefined -- \x -> (fmap (*1) <$> x, fmap ((+1).(*2)) <$> x, 0)

-- r4 :: Rec ((XX k, XX k, Integer), (XX k, XX k, Integer), Integer)
-- r4 = undefined

-- r4' :: Rec (XX k, XX k, Integer)
-- r4' = undefined


