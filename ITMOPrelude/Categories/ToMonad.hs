{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.ToMonad where
import ITMOPrelude.Categories

import ITMOPrelude.Categories.MonadJoin
import ITMOPrelude.Categories.MonadFish

instance MonadJoin m => Monad m where
  return = returnJoin
  f >>= g = join (fmap g f)