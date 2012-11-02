{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.ToMonad where
import ITMOPrelude.Categories.MonadFish

import ITMOPrelude.Categories
import ITMOPrelude.Categories.MonadJoin

instance Monad m => MonadFish m where
  returnFish = return
  f >=> g = \a -> f a >>= g
