{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPredule.Algebra where

-- Классы
class Monoid a where
    mempty :: a
    mappend :: a -> a -> a

class Monoid g => Group g where
	gempty :: g
	ginv :: g -> g
	gappend :: g -> g -> g
