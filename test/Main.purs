module Test.Main where

import Prelude hiding (discard)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Data.Foldable (class Foldable)
import Data.List ((..))

import Pipes (each, for, (>->), discard)
import Pipes.Core (runEffectRec, Pipe)
import Pipes.Prelude as P

runDrain :: forall e f a. Foldable f => Pipe Int a (Eff e) Unit -> f Int -> (Eff e) Unit
runDrain p v = runEffectRec $ for (each v >-> p) discard

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = runDrain (P.map (add 2)) (1..3000)
