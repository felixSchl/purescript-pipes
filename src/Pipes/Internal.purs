module Pipes.Internal where

import Prelude
import Data.Monoid (class Monoid, mempty)
import Control.Monad.Trans (class MonadTrans)

data Proxy a' a b' b m r
  = Request a' (a  -> Proxy a' a b' b m r)
  | Respond b  (b' -> Proxy a' a b' b m r)
  | M          (m    (Proxy a' a b' b m r))
  | Pure       r

instance functorProxy :: (Monad m) => Functor (Proxy a' a b' b m) where
  map f p0 = go p0 where
    go p = case p of
      Request a' fa  -> Request a' (go <<< fa)
      Respond b  fb' -> Respond b  (go <<< fb')
      M           m  -> M          (go <$> m)
      Pure        r  -> Pure       (f r)

instance applyProxy :: (Monad m) => Apply (Proxy a' a b' b m) where
  apply pf0 px = go pf0 where
    go pf = case pf of
      Request a' fa  -> Request a' (go <<< fa)
      Respond b  fb' -> Respond b  (go <<< fb')
      M           m  -> M          (go <$> m)
      Pure        f  -> f <$> px

instance applicativeProxy :: (Monad m) => Applicative (Proxy a' a b' b m) where
  pure = Pure

instance bindProxy :: (Monad m) => Bind (Proxy a' a b' b m) where
  bind p0 f = go p0 where
    go p = case p of
      Request a' fa  -> Request a' (go <<< fa)
      Respond b  fb' -> Respond b  (go <<< fb')
      M           m  -> M          (go <$> m)
      Pure        r  -> f r

instance monoidProxy :: (Monad m, Monoid r) => Monoid (Proxy a' a b' b m r) where
  mempty = Pure mempty

instance semigroupProxy :: (Monad m, Semigroup r) => Semigroup (Proxy a' a b' b m r) where
  append p1 p2 = go p1 where
    go p = case p of
      Request a' fa  -> Request a' (go <<< fa)
      Respond b  fb' -> Respond b  (go <<< fb')
      M           m  -> M          (go <$> m)
      Pure       r1  -> (r1 <> _) <$> p2

instance monadTransProxy :: MonadTrans (Proxy a' a b' b) where
  lift m = M (Pure <$> m)

observe :: forall m a' a b' b m r
        .  Monad m => Proxy a' a b' b m r -> Proxy a' a b' b m r
observe p0 = M (go p0) where
    go p = case p of
        Request a' fa  -> return (Request a' (observe <<< fa))
        Respond b  fb' -> return (Respond b  (observe <<< fb'))
        M           m  -> m >>= go
        Pure        r  -> return (Pure r)

newtype X = X X

closed :: forall a. X -> a
closed (X x) = closed x
