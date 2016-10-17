{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeInType #-}

module Data.X.Folding where

import Data.Kind
import Data.Tree
import Data.Typeable
import Data.X
import GHC.TypeLits( ErrorMessage(..), TypeError )
import Data.Type.Equality
import Data.Type.Bool
import Unsafe.Coerce
import Data.X.Pair


-- -- | Unit type with @'`Passes` :: `Passes`@ for assertions.
-- data Passes = Passes deriving (Eq, Ord, Show)


-- -- | Assert that @`FoldX` (`UnfoldX` a) == `X` a@
-- type family TestUnfoldFold (a :: k) :: Passes where
--   TestUnfoldFold a = AssertEq (FoldX (UnfoldX a)) (X a)



-- | Recursively unfold a type
type family UnfoldX (a :: k) :: Type where
  UnfoldX a = UnfoldXL VoidX (X a)

-- | Recursively unfold a type with given argument list
type family UnfoldXL (l :: Type) (a :: Type) :: Type where
  UnfoldXL l (X (X Y)) =                 X (X Y) .: l
  UnfoldXL l (X (c a)) = UnfoldXL (UnfoldX (a  ) .: l) (X c) -- .|| (X c .: (UnfoldX a .: l))
  UnfoldXL l (X  c   ) =                 X (c  ) .: l

-- | Recursively fold a (X type :. type list)
-- Should have:
-- @`FoldX` (`UnfoldX` a) == `X` a@
type family FoldX (a :: Type) = (b :: Type) where
  FoldX (X (c :: k)       .:   (VoidX  )) =       (X (c :: k)                )
  FoldX (X (c :: k)       .:   (a .: b )) = FoldX (X (c :: k) .$ FoldX a .: b)
  FoldX (X (c :: k0 -> k) .: X (a :: k0)) =       (X (c    a)                )
  FoldX (X (c :: k)                     ) =       (X (c :: k)                )

-- | This is like fromJust. It should always succeed, as long
-- as its argument is exactly the format output by `UnfoldX`.
type family FoldXType (a :: Type) :: Type where
  FoldXType (X (c :: Type) .:  VoidX  ) =               c
  FoldXType (X (c :: k   ) .:  VoidX  ) = TypeError ('Text "FoldXType: Type " ':<>: 'ShowType c ':<>: 'Text "does not have kind *.")
  FoldXType (X (c        ) .: (a .: b)) = FoldXType (X (c (FoldXType a)) .: b)


-- | Show an unfolded type in tree form
showUnfolded :: ToTree (UnfoldX a) => a -> String
showUnfolded = drawTree . toTree . (unsafeCoerce :: a -> UnfoldX a)

-- | Print a type unfolded and in tree form
printUnfolded :: ToTree (UnfoldX a) => a -> IO ()
printUnfolded = putStrLn . showUnfolded

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




