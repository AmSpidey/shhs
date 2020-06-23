{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Utils where

import Abs

import qualified Data.Map as Map

import Control.Monad.Reader
import UnliftIO

type VarReader m = (MonadReader (IORef ShellState) m, MonadIO m)

-- | Variables handling.

setVar :: String -> Val -> Shell ()
setVar name val = do
  stRef <- ask
  modifyIORef stRef $ \st -> st {shellStEnv = Map.insert name val $ shellStEnv st}

getVar :: VarReader m => String -> m (Maybe Val)
getVar name = do
 stRef <- ask
 st <- readIORef stRef
 return $ Map.lookup name $ shellStEnv st

-- | The following 4 functions are adapted from the `extra` package.

-- | Like 'when', but where the test can be monadic.
whenM :: Monad m => m Bool -> m () -> m ()
whenM b t = ifM b t (pure ())

-- | Like 'unless', but where the test can be monadic.
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b = ifM b (pure ())

-- | Like @if@, but where the test can be monadic.
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b' <- b; if b' then t else f

-- | Like 'not', but where the test can be monadic.
notM :: Functor m => m Bool -> m Bool
notM = fmap not
