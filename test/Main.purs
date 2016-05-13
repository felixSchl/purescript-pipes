module Test.Main where

import Prelude
import Debug.Trace
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Pipes
import Pipes.Core
import Pipes.Internal

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  x <- runEffect (yield unit)
  traceShowA x
  log "You should add some tests."
