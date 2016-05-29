module Pipes.ListT where

import Prelude
import Control.Alt (class Alt, alt)
import Control.Alternative (class Alternative)
import Control.Apply ((*>))
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Except.Trans
    (class MonadError, class MonadTrans, lift, catchError, throwError)
import Control.Monad.Reader.Class (class MonadReader, local, ask)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State.Class (class MonadState, state)
import Control.Monad.Writer.Class (class MonadWriter, listen, pass, writer)
import Control.MonadPlus (class MonadPlus)
import Control.Plus (class Plus, empty)
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(Tuple))

import Pipes (yield, for)
import Pipes.Core (Producer, runEffect, runEffectRec)
import Pipes.Internal (Proxy(Pure, M, Respond, Request))


newtype ListT m a = Select (Producer a m Unit)

enumerate :: forall a m. ListT m a -> Producer a m Unit
enumerate (Select l) = l

runListT :: forall a m. (Monad m) => ListT m a -> m Unit
runListT l = runEffect (enumerate (l *> empty))

runListTRec :: forall a m. (MonadRec m) => ListT m a -> m Unit
runListTRec l = runEffectRec (enumerate (l *> empty))

instance listTFunctor :: (Monad m) => Functor (ListT m) where
    map f (Select p) = Select (for p (yield <<< f))

instance listTApply :: (Monad m) => Apply (ListT m) where
    apply (Select mf) (Select mx) = Select (for mf (\f -> for mx (\x -> yield (f x))))

instance listTApplicative :: (Monad m) => Applicative (ListT m) where
    pure = Select <<< yield

instance listTBind :: (Monad m) => Bind (ListT m) where
    bind (Select p1) f = Select (for p1 (enumerate <<< f))

instance listTMonad :: (Monad m) => Monad (ListT m)

instance listTMonadTrans :: MonadTrans ListT where
    lift m = Select (lift m >>= yield)

instance listTAlt :: (Monad m) => Alt (ListT m) where
    alt (Select p1) (Select p2) = Select (p1 *> p2)

instance listTPlus :: (Monad m) => Plus (ListT m) where
    empty = Select (return unit)

instance listTAlternative :: (Monad m) => Alternative (ListT m)

instance listTMonadPlus :: (Monad m) => MonadPlus (ListT m)

instance listTMonadEff :: (MonadEff eff m) => MonadEff eff (ListT m) where
    liftEff = lift <<< liftEff

instance listTSemigroup :: (Monad m) => Semigroup (ListT m a) where
    append = alt

instance listTMonoid :: (Monad m) => Monoid (ListT m a) where
    mempty = empty

instance listTMonadState :: (MonadState s m) => MonadState s (ListT m) where
    state = lift <<< state

instance listTMonadWriter :: (Monoid w, MonadWriter w m) => MonadWriter w (ListT m) where
    writer = lift <<< writer

    listen (Select p) = Select (go p mempty)
        where
        go (Request a' fa) w = Request a' (\a -> go (fa a) w)
        go (Respond b fb') w = Respond (Tuple b w) (\b' -> go (fb' b') w)
        go (M m)           w = M (do Tuple p' w' <- listen m
                                     return (go p' (append w w')))
        go (Pure r)        w = Pure r

    pass (Select p) = Select (go p mempty)
        where
        go (Request a' fa)           w = Request a' (\a -> go (fa a) w)
        go (Respond (Tuple b f) fb') w = M (pass (return (Tuple _1 _2)))
                                         where _1 = Respond b (\b' -> go (fb' b') (f w))
                                               _2 = \_ -> f w
        go (M m)                     w = M (do Tuple p' w' <- listen m
                                               return (go p' (append w w')))
        go (Pure r)                  w = Pure r

instance listTMonadReader :: (MonadReader r m) => MonadReader r (ListT m) where
    ask = lift ask
    local f (Select l) = Select (local f l)

instance listTMonadError :: (MonadError e m) => MonadError e (ListT m) where
    throwError = lift <<< throwError
    catchError (Select l) f = Select (l `catchError` (enumerate <<< f))