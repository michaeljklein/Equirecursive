{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- {-# LANGUAGE IncoherentInstances #-}

module Data.Type.Test.Assert where


import Control.Monad
import Data.Exists
import Data.IntSet (IntSet)
import Data.Kind
import Data.Proxy
import Data.Sequence (Seq)
import Data.Type.Equality
import Data.Type.Bool
import Data.Typeable
import Data.Word
import Data.X
import GHC.TypeLits
import Numeric.Natural
import Test.QuickCheck
import Test.QuickCheck.Poly
import Control.Comonad
import Data.Foldable (toList)
import Data.Default
import Test.QuickCheck.Property (succeeded)
import Data.X.Drop

import Unsafe.Coerce
import System.IO.Unsafe
import System.Mem.StableName
import Data.Type.Test.ShrinkType
import GHC.Prim

-- TODO: Try: instead of x `seq` etc, try typeOf x `seq` etc.
-- This should prevent `X` from being evaluated, but sill catch type errors?

-- | Unit type with @'`Passes` :: `Passes`@ for assertions.
data Passes = Passes deriving (Eq, Ord, Show, Typeable)

data ResultK = SuccessK | FailK ErrorMessage deriving (Typeable)

-- | Fun note: to see the contents of `FailK`, you must
-- first `dropX` it.
instance Show ResultK where
  show (SuccessK) = "SuccessK"
  show (FailK  _) = "FailK [dropX to show]"

type instance (==) (a :: ResultK) (b :: ResultK) = ResultKEq a b

type family ResultKEq (a :: ResultK) (b :: ResultK) :: Bool where
  'SuccessK `ResultKEq` 'SuccessK = 'True
  'SuccessK `ResultKEq` 'FailK b  = 'False
  'FailK a  `ResultKEq` 'SuccessK = 'False
  'FailK a  `ResultKEq` 'FailK b  = a == b

data AResult = forall (r :: ResultK). DropX r => AResult { getAResult :: X r }

-- type AResult = ExistsK ResultK DropBool

instance DropX 'SuccessK where
  type   Drop  'SuccessK = ResultK
  dropX _ = SuccessK

instance TypeError e => DropX ('FailK e) where
  type Drop ('FailK e) = ResultK
  dropX _ = FailK undefined


eqn :: ExistsK k c -> Exists Nice
eqn (ExistsK x0) = ExistsK (return (eqnX x0))

eqnX :: X (a :: k) -> Bool
eqnX x = eqn_ x `seq` eqn_ x

eqn_ :: a -> Bool
eqn_ = (\x -> let n = unsafePerformIO (makeStableName x) in eqStableName n n)

-- | Results in the given `TypeError` if `False`
type family AssertR (b :: Bool) (e :: ErrorMessage) :: ResultK where
  AssertR 'True  e = 'SuccessK
  AssertR 'False e = 'FailK e

type family (===) (a :: k) (b :: k) :: ResultK where
  a === b = AssertR (a == b) ('Text "Data.Type.Test.Assert.(===): Arguments are not equal:" ':<>: ShowType2 a b)

class Fl (a :: ResultK)
instance Fl 'SuccessK
instance TypeError e => Fl ('FailK e)
-- instance TypeError ('Text "I'm not sure how these work:" ':<>: 'ShowType a) => Fl (a :: ResultK)


testAssert :: ExistsK Bool NiceX -> ExistsK Bool NiceX -> ExistsK Bool ()
testAssert (ExistsK x0) (ExistsK x1) = ExistsK (testAssert_ x0 x1)

testAssert_ :: X (a :: Bool) -> X (b :: Bool) -> X (a == b)
testAssert_ = def


-- Consider the following class:

-- class GeneralConstraints a0 .. an => C (a0 :: k0) .. (an :: kn) where
--   method :: proxy a0 -> ..

-- instance ConstraintsTrue a1 .. an => C ('True :: Bool) .. (an :: kn) where
--   method :: proxy 'True -> ..

-- instance ConstraintsFalse a1 .. an => C ('False :: Bool) .. (an :: kn) where
--   method :: proxy 'False -> ..

-- Now we have everything we need to make an instance:

-- instance (ConstraintsTrue a1 .. an, ConstraintsFalse a1' .. an', a1 ~ a1' .. an ~ an') => C (bool :: Bool) .. (an :: kn) where
--   method :: forall (bool :: Bool). proxy (bool :: Bool) -> ..

-- However, GHC currently cannot make this implication.

-- I propose that two typechecker additions be made:

-- (Given a datatype D = D0 d00 .. d0a | D1 d10 .. d1b | .. | DN ..)

-- 1) (C (D0 ..), C (D1 ..), .., C (DN ..)) => forall (d :: D). C (d :: D)
--   In other words, having instances for all types in a sum of kind k
--   implies instances for all types of kind k

-- 2) method :: (C (D0 ..), .., C (DN ..)) => forall (d :: D). proxy a0 -> ..


-- -------------------

-- class C (a :: Bool) where
--   cfunc :: proxy a -> Stuff

-- instance C 'True where
--   cfunc = ..

-- instance C 'False where
--   cfunc = ..

-- cfunc :: forall (a :: Bool). proxy a -> Stuff

-- -------------------

-- (Typeable a, Typeable b) => Typeable (a == b)





-- instance Testable AResult where
--   property = property . isSuccess




-- | Bus error: 10, from the following:
-- (\(ExistsK x) -> typeOf x) (boolsDrop' (ExistsK (def :: X 'True)))
boolsDrop' :: ExistsK Bool DropX -> ExistsK Bool Typeable
boolsDrop' = unsafeCoerce



instance Default Passes where
  def = Passes

type family PassesEq (a :: Passes) (b :: Passes) :: Bool where
  PassesEq 'Passes 'Passes = 'True
  PassesEq  a       b      = TypeError ('Text "Somehow the following were interpreted as having kind Passes:" ':<>: ShowType2 a b)

type instance (==) (a :: Passes) (b :: Passes) = PassesEq a b

instance Testable APasses where
  property (APasses x) = property succeeded

class ((a == 'Passes) ~ eq) => ShowPasses (a :: Passes) (eq :: Bool) where
  showPasses :: X a -> String

-- | This is probably superflous, but I think it might not be.
data APasses = forall (a :: Passes). Typeable a => APasses { getAPasses :: X a }

-- | Trivial instance, mostly to make sure `APasses` is fully evaluated.
instance Show APasses where
  show (APasses xa) = show ((def :: X (a :: Passes) -> X 'Passes) xa)

instance Default APasses where
  def = APasses (def :: X ('Passes :: Passes))


-- | Results in the given `TypeError` if `False`
type family Assert (b :: Bool) (e :: ErrorMessage) :: Passes
type instance Assert 'True  e = 'Passes
type instance Assert 'False e = TypeError e

-- | Show two types
type family ShowType2 (a0 :: k0) (a1 :: k1) :: ErrorMessage where
  ShowType2 a0 a1 = 'Text "\n  " ':<>: 'ShowType a0 ':<>: 'Text "\n  " ':<>: 'ShowType a1 ':<>: 'Text "\n"

-- | Results in a `TypeError` if `/=`. Reports its arguments upon error.
type family AssertEq (a :: k) (b :: k) :: Passes where
  AssertEq a b = Assert (a == b) ('Text "AssertEq failed with arguments:" ':<>: ShowType2 a b)

-- | Results in a `TypeError` if `/=`. Reports its arguments upon error.
type family AssertNotEq (a :: k) (b :: k) :: Passes where
  AssertNotEq a b = Assert (Not (a == b)) ('Text "AssertEq failed with arguments:" ':<>: ShowType2 a b)

-- | @a@ implies @b@
type family (==>) (a :: Bool) (b :: Bool) :: Passes where
  'True  ==> 'True  = 'Passes
  'True  ==> 'False =  TypeError ('Text "Implication (==>) failed")
  'False ==> 'True  = 'Passes
  'False ==> 'False = 'Passes

-- -- | Alias for `AssertEq`
-- type family (===) (a :: k) (b :: k) :: Passes where
--   a === b = AssertEq a b

-- | Alias for `AssertNotEq`
type family (/==) (a :: k) (b :: k) :: Passes where
  a /== b = AssertNotEq a b

