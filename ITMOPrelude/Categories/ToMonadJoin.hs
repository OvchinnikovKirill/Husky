{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.ToMonad where
import ITMOPrelude.Categories.MonadJoin
import ITMOPrelude.Primitive

import ITMOPrelude.Categories
import ITMOPrelude.Categories.MonadFish

instance MonadFish m => Functor m where
  fmap f x = (id >=> (returnFish . f)) x

instance MonadFish m => MonadJoin m where
  returnJoin = returnFish
  join ma = (id >=> id) ma