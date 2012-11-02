{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Categories where

import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Tree


class Category cat where
	id :: cat a a
	(.) :: cat b c -> cat a b -> cat a c

class Functor f where
	fmap :: (a -> b) -> f a -> f b

class Monad m where
	return :: a -> m a
	(>>=) :: m a -> (a -> m b) -> m b
  
 
instance Category (->) where
    id = \x -> x
    (.) f g x = f (g x)

instance Functor List where
	fmap = map

instance Monad List where
	return x = Cons x Nil
	xs >>= f = concatMap f xs
  
instance Functor Maybe where
	fmap f (Just a) = Just $ f a
	fmap f Nothing = Nothing

instance Monad Maybe where
	return = Just
	Nothing >>= _ = Nothing
	(Just x) >>= f = f x

instance Functor Tree where
	fmap = tmap


newtype State s a = State { runState :: s -> (s, a) }

instance Monad (State s) where
	return a = State $ \s -> (s, a)
	state1 >>= f = State $ \s -> let (state2, a) = runState state1 s
							in runState (f a) state2