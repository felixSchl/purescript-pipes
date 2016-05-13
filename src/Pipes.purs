module Pipes where

import Prelude
import Pipes.Core
import Pipes.Internal (Proxy(..))

infixl 4 composeLoopBodies' as <~
infixr 4 composeLoopBodies  as ~>
infixr 5 replaceAwait       as >~
infixl 5 replaceAwait'      as ~<
-- infixl 7 composePipes       as >->
-- infixr 7 composePipes'      as <-<
--
for
  :: forall a a' b b' c c' x x' m
   . Monad m
  =>       Proxy x' x b' b m a'
  -> (b -> Proxy x' x c' c m b')
  ->       Proxy x' x c' c m a'
for = (//>)

-- (~>)
composeLoopBodies
  :: forall a a' b b' c c' x x' m
   . Monad m
  => (a -> Proxy x' x b' b m a')
  -> (b -> Proxy x' x c' c m b')
  -> (a -> Proxy x' x c' c m a')
composeLoopBodies = (/>/)

-- (<~)
composeLoopBodies'
  :: forall a a' b b' c c' x x' m
   . Monad m
  => (b -> Proxy x' x c' c m b')
  -> (a -> Proxy x' x b' b m a')
  -> (a -> Proxy x' x c' c m a')
composeLoopBodies' = flip composeLoopBodies

await :: forall a m. Monad m => Consumer a m a
await = request unit

-- (~<)
replaceAwait
  :: forall a a' b b' y y' c m
   . Monad m
  => Proxy a'   a y' y m b
  -> Proxy Unit b y' y m c
  -> Proxy a'   a y' y m c
replaceAwait p1 p2 = const p1 >\\ p2

-- (>~)
replaceAwait'
  :: forall a a' b b' y y' c m
   . Monad m
  => Proxy Unit b y' y m c
  -> Proxy a'   a y' y m b
  -> Proxy a'   a y' y m c
replaceAwait' = flip replaceAwait

cat :: forall a m r. Monad m => Pipe a a m r
cat = pull unit

-- (>->)
composePipes
  :: forall a a' b c c' m r
   . Monad m
  => Proxy a'   a Unit b m r
  -> Proxy Unit b c'   c m r
  -> Proxy a'   a c'   c m r
composePipes p1 p2 = const p1 +>> p2

-- (<-<)
composePipes'
  :: forall a a' b c c' m r
   . Monad m
  => Proxy Unit b c'   c m r
  -> Proxy a'   a Unit b m r
  -> Proxy a'   a c'   c m r
composePipes' = flip composePipes

yield :: forall m a. Monad m => a -> Producer a m Unit
yield = respond
