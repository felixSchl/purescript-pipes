module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Pipes (yield)
import Pipes.Core ((//>), runEffect, Producer_)

type Result = Int

myLog
  :: forall m
   . Monad m
  => String
  -> Producer_ String m Unit
myLog = yield

foo
  :: forall eff
   . Int  
  -> Int
  -> Producer_ String (Eff eff) Result
foo x y = do
  myLog $ "About to compute: " <> show x <> " + " <> show y
  r <- pure $ x + y
  myLog $ "Computed: " <> show x <> " + " <> show y <> " = " <> show r
  pure r

main :: Eff (console :: CONSOLE) Unit
main = do
  r <- runEffect $ (foo 1 2) //> (liftEff <<< log <<< append "Log >>> ")
  log $ "Result >>> " <> show r
