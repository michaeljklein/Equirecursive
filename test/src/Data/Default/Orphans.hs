module Data.Default.Orphans where

import Data.Default
import Test.QuickCheck.Poly
import Numeric.Natural

-- | Returns `False`
instance Default Bool where
  def = False

-- | Returns @`Left` `def`@
instance Default a => Default (Either a t) where
  def = Left def

-- | Returns NULL
instance Default Char where
  def = '\0'

-- | Returns @0@
instance Default Natural where
  def = 0

-- | Returns @0@
instance Default A where
  def = A 0

-- | Returns @0@
instance Default B where
  def = B 0

-- | Returns @0@
instance Default C where
  def = C 0

-- | Returns @0@
instance Default OrdA where
  def = OrdA 0

-- | Returns @0@
instance Default OrdB where
  def = OrdB 0

-- | Returns @0@
instance Default OrdC where
  def = OrdC 0

