module Example where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (randomInt, RANDOM)

import Pipes (yield, for)
import Pipes.Core (runEffect, Producer_)

pipedRandomInt
  :: ∀ eff
   . Int
  -> Int
  -> Producer_ String (Eff (random :: RANDOM | eff)) Int
pipedRandomInt x y = do
  yield $ "Generating an Int between " <> show x <> " and " <> show y <> "..."
  r <- liftEff $ randomInt x y
  yield $ "Generated " <> show r
  pure r

main :: Eff (console :: CONSOLE, random :: RANDOM) Unit
main =
  let go = pipedRandomInt 1 10
   in do
    r <- runEffect $ for go (liftEff <<< log <<< ("Log: " <> _))
    log $ "Result: " <> show r
