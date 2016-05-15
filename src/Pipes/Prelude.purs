module Pipes.Prelude where

import Prelude
import Pipes
import Pipes.Core
import Pipes.Internal
import Data.Foldable (class Foldable)
import Control.Monad.Trans (lift)

-- | Repeat a monadic action indefinitely, 'yield'ing each result
repeatM :: forall a m r. Monad m => m a -> Producer_ a m r
repeatM m = lift m >~ cat

-- | Repeat a monadic action a fixed number of times, 'yield'ing each result
replicateM :: forall a m. Monad m => Int -> m a -> Producer_ a m Unit
replicateM n m = lift m >~ take n

-- | take n only allows n values to pass through
take :: forall a m. Monad m => Int -> Pipe a a m Unit
take = loop where
  loop 0 = return unit
  loop n = do
    a <- await
    yield a
    loop (n - 1)

-- | Consume all values using a monadic function
mapM_ :: forall a m r. Monad m => (a -> m Unit) -> Consumer_ a m r
mapM_ f = for cat (\a -> lift (f a))

-- | 'discard' all incoming values
drain :: forall a m r. Monad m => Consumer_ a m r
drain = for cat discard

-- | Apply a function to all values flowing downstream
map :: forall a b m r. Monad m => (a -> b) -> Pipe a b m r
map f = for cat (\a -> yield (f a))

-- | Apply a monadic function to all values flowing downstream
mapM :: forall a b m r. Monad m => (a -> m b) -> Pipe a b m r
mapM f = for cat $ \a -> do
    b <- lift (f a)
    yield b

-- | Convert a stream of actions to a stream of values
sequence :: forall a m r. Monad m => Pipe (m a) a m r
sequence = mapM id

{- | Apply a function to all values flowing downstream, and
     forward each element of the result.
-}
mapFoldable :: forall a b m t r
             . (Monad m, Foldable t)
            => (a -> t b)
            -> Pipe a b m r
mapFoldable f = for cat (\a -> each (f a))
