module Pipes.Safe where

import Debug.Trace
import Prelude
import Pipes
import Pipes.Core
import Pipes.Internal

mask
  :: forall a a' b b' m r
   . (Monad m)
  => ((forall x. Proxy a' a b' b m x -> Proxy a' a b' b m x)
        -> Proxy a' a b' b m r)
  -> Proxy a' a b' b m r
mask restore = do
  loop $ restore unmask
  where
    loop
      :: forall a a' b b' m r
       . (Monad m)
      => Proxy a' a b' b m r
      -> Proxy a' a b' b m r
    loop (Request a' fa ) = Request a' (loop <<< fa )
    loop (Respond b  fb') = Respond b  (loop <<< fb')
    loop (M m)            = M $ do
      traceShowA "stash base loop"
      m >>= chunk >>= return <<< loop
    loop (Pure r)         = Pure r

    unmask
      :: forall a a' b b' m r
       . (Monad m)
      => Proxy a' a b' b m r
      -> Proxy a' a b' b m r
    unmask (Request a' fa ) = Request a' (unmask <<< fa )
    unmask (Respond b  fb') = Respond b  (unmask <<< fb')
    unmask (M m)            = M $ do
      traceShowA "retrieve base unmask"
      m >>= chunk >>= return <<< unmask
    unmask (Pure r)         = Pure r

    chunk
      :: forall a a' b b' m s
       . (Monad m)
      => Proxy a' a b' b m s
      -> m (Proxy a' a b' b m s)
    chunk (M m) = m >>= chunk
    chunk s     = return s
