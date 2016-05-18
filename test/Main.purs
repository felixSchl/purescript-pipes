module Test.Main where

import Prelude
import Debug.Trace
import Data.Identity
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Pipes
import Pipes.Core
import Pipes.Internal
import Pipes.Prelude
import Pipes.Safe

foo :: Producer Int (Eff _) Unit
foo = mask \restore -> do
  traceShowA "GOING..."
  restore $ go 0
  traceShowA "DONE!"

  where
    go n = do
      yield n
      go (n + 1)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  runEffect $ for (foo >-> (take 3)) traceShowA

  log "You should add some tests."
