module Data.Equirecursive.Graph where

import Data.Equirecursive

-- Likely will reference Data.Equirecursive.Map?

-- TODO: How does this work with state transformers??
-- Really need to figure out equirecursive type state transformations

-- newtype Vertex a = Vertex { getVertex :: RecurseL ([Vertex a],

-- newtype Graph a = ([Graph a], a)

-- newtype EGraph k a = EGraph { getEGraph :: RecurseL (Map k (a, XY)) }

-- instance Ord k => Functor (EGraph k)
-- instance Ord k => Applicative (EGraph k)
-- instance Ord k => Comonad (EGraph k)
