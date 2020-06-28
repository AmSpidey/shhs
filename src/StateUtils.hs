{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module StateUtils where

import System.Exit

import Control.Monad.Reader

import Data.Map ((!))
import qualified Data.Map as Map

import UnliftIO

import Abs

-- TODO: make all functions use this synonym

type ShM m = (MonadReader (IORef ShellState) m, MonadIO m)


-- TODO: those [g|s]etters are similar, merge them somehow? Probably would require TemplateHaskell tho...
setVar :: String -> Val -> Shell ()
setVar name val = do
  stRef <- ask
  modifyIORef stRef $ \st -> st {shellStEnv = Map.insert name val $ shellStEnv st}

getVar :: ShM m => String -> m (Maybe Val)
getVar name = do
 stRef <- ask
 st <- readIORef stRef
 return $ Map.lookup name $ shellStEnv st


getPath :: Shell Path
getPath = do
  stRef <- ask
  st <- readIORef stRef
  return $ shellStPath st

setPath :: Path -> Shell ()
setPath p = do
  stRef <- ask
  modifyIORef stRef $ \st -> st {shellStPath = p}

setErrCode :: ExitCode -> Shell ()
setErrCode i = do
  stRef <- ask
  modifyIORef stRef $ \st -> st {shellLastErrCode = i}

addAlias :: String -> String -> Shell ()
addAlias a v = do
  stRef <- ask
  modifyIORef stRef $ \st -> st {shellAliases = Map.insert a v $ shellAliases st}

getAliases :: ShM m => m AliasMap
getAliases = do
  stRef <- ask
  st <- readIORef stRef
  return $ shellAliases st

isAlias :: ShM m => String -> m Bool
isAlias s = (s `Map.member`) <$> getAliases

-- Warning: non total. Only call when sure @s is in alias map.
getAlias :: ShM m => String -> m String
getAlias s = (! s) <$> getAliases

getConfig :: Shell ProcessConfig
getConfig = do
  stRef <- ask
  st <- readIORef stRef
  return $ processConfig st

setConfig :: (ProcessConfig -> ProcessConfig) -> Shell ()
setConfig modifier = do
  stRef <- ask
  modifyIORef stRef $ \st -> st {processConfig = modifier $ processConfig st}

emptyConfig :: ProcessConfig
emptyConfig = ProcessConfig { stdinPath = Nothing, stdoutPath = Nothing, stderrPath = Nothing}
