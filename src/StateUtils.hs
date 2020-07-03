{-# LANGUAGE ConstraintKinds, FlexibleContexts, OverloadedStrings, BlockArguments #-}
module StateUtils where

import System.Exit

import Control.Monad.Reader

import Data.Maybe
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Text as T

import UnliftIO

import Abs
import Utils

-- TODO: make all functions use this synonym - otherwise they can't be used in parser

type ShM m = (MonadReader (IORef ShellState) m, MonadIO m)

getRef :: ShM m => m (IORef ShellState)
getRef = ask


setVar :: String -> Val -> Shell ()
setVar name val = do
  stRef <- getRef
  modifyIORef stRef $ \st -> st{shellStEnv = Map.insert name val $ shellStEnv st}

getVar :: ShM m => String -> m (Maybe Val)
getVar name = do
 stRef <- getRef
 st <- readIORef stRef
 return $ Map.lookup name $ shellStEnv st

getVarStr :: String -> Shell String
getVarStr name = do
  val <- getVar name
  return $ show $ fromMaybe (VStr "") val

getActiveJob :: Shell (Maybe Job)
getActiveJob = do
  stRef <- getRef
  st <- readIORef stRef
  return $ activeJob st

finishJob :: Shell ()
finishJob = do
  stRef <- getRef
  modifyIORef stRef \st -> st{activeJob = Nothing}

getActiveProcs :: Shell (Map Job [Async ExitCode])
getActiveProcs = do
  stRef <- getRef
  st <- readIORef stRef
  return $ activeProcesses st

getJobProcs :: Shell [Async ExitCode]
getJobProcs = do
  m <- getActiveProcs
  mj <- getActiveJob
  return $ case mj of
             Nothing -> []
             Just j -> m ! j


startJob :: Shell Job
startJob = do
  stRef <- getRef
  job <- generateJobID
  -- TODO: make this wait for previous job completion? or get rid of activeJob and pass id by hand
  mj <- getActiveJob
  when (isJust mj) $ error $ "this is hsh bug :(" ++ show mj
  modifyIORef stRef $ \st -> st{activeJob = Just job, activeProcesses = Map.insert job [] $ activeProcesses st}
  return job

generateJobID :: Shell Job
generateJobID = do
  s <- Job <$> randomString 20
  m <- getActiveProcs
  if s `Map.member` m
    then generateJobID
    else return s

addActiveProc :: Async ExitCode -> Shell ()
addActiveProc a = do
  stRef <- getRef
  mj <- getActiveJob
  when (isJust mj) $
    modifyIORef stRef \st -> st{activeProcesses = Map.adjust (a:) (fromJust mj) $ activeProcesses st}


getPath :: Shell Path
getPath = do
  stRef <- getRef
  st <- readIORef stRef
  return $ shellStPath st

setPath :: Path -> Shell ()
setPath p = do
  stRef <- getRef
  setVar "PWD" (strToVal p)
  modifyIORef stRef $ \st -> st {shellStPath = p}

setErrCode :: ExitCode -> Shell ()
setErrCode i = do
  stRef <- getRef
  modifyIORef stRef $ \st -> st{shellLastErrCode = i}
  setVar "EXITCODE" $ VInt $ case i of
                             ExitSuccess -> 0
                             ExitFailure err -> toInteger err

addAlias :: String -> String -> Shell ()
addAlias a v = do
  stRef <- getRef
  modifyIORef stRef $ \st -> st{shellAliases = Map.insert a v $ shellAliases st}

getAliases :: ShM m => m AliasMap
getAliases = do
  stRef <- getRef
  st <- readIORef stRef
  return $ shellAliases st

isAlias :: ShM m => String -> m Bool
isAlias s = (s `Map.member`) <$> getAliases

-- Warning: non total. Only call when sure @s is in alias map.
getAlias :: ShM m => String -> m String
getAlias s = (! s) <$> getAliases

getConfig :: Shell PIOConfig
getConfig = do
  stRef <- getRef
  st <- readIORef stRef
  return $ processConfig st

setConfig :: PIOConfig -> Shell ()
setConfig = modifyConfig . const

modifyConfig :: (PIOConfig -> PIOConfig) -> Shell ()
modifyConfig modifier = do
  stRef <- getRef
  modifyIORef stRef \st -> st{processConfig = modifier $ processConfig st}

setRPath :: RedirectType -> Path -> Shell ()
setRPath RStdin = setStdinPath
setRPath RStdout = setStdoutPath
setRPath RStderr = setStderrPath

setStdinPath :: Path -> Shell ()
setStdinPath = setStdinH . HPath

setStdoutPath :: Path -> Shell ()
setStdoutPath = setStdoutH . HPath

setStderrPath :: Path -> Shell ()
setStderrPath path = modifyConfig \c -> c{stderrH = HPath path}

setStdinHandle :: Handle -> Shell ()
setStdinHandle = setStdinH . HHandle

setStdoutH :: HsHandle -> Shell ()
setStdoutH h = modifyConfig \c -> c{stdoutH = h}

setStdinH :: HsHandle -> Shell ()
setStdinH h = modifyConfig \c -> c{stdinH = h}

clearConfig :: Shell ()
clearConfig = setConfig def

withSavedConfig :: Shell a -> Shell a
withSavedConfig m = do
  conf <- getConfig
  m `finally` setConfig conf

strToVal :: String -> Val
strToVal = VStr . T.pack

