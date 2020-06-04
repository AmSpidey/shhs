{-# LANGUAGE OverloadedStrings #-}
module Shell where

import System.Exit
import System.Environment
import System.Directory
import System.Process.Typed
import System.Console.ANSI.Codes
import System.Console.Haskeline

import Data.Colour.SRGB
import Data.Maybe
import Data.Word (Word8)
import Data.List
import qualified Data.Text.Read as TR
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as Map

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.IO.Class
import qualified Control.Exception as E
import UnliftIO

import Abs
import Parser
import Utils

-- Expressions evaluation

add :: Val -> Val -> Except Err Val
add (VInt a) (VInt b) = return $ VInt $ a + b
add _ _ = throwError "Wrong type of arguments for addition"

subt :: Val -> Val -> Except Err Val
subt (VInt a) (VInt b) = return $ VInt $ a - b
subt _ _= throwError "Wrong type of arguments for subtraction"

mul :: Val -> Val -> Except Err Val
mul (VInt a) (VInt b) = return $ VInt $ a * b
mul _ _ = throwError "Wrong type of arguments for multiplication"

evalExpr :: Expr -> Except Err Val
evalExpr (Lit s) = return $ VStr s
evalExpr (Int i) = return $ VInt i
evalExpr (Negation expr) = evalExpr $ Product (Int (-1)) expr
evalExpr (Sum expr1 expr2) = do
  val1 <- evalExpr expr1
  val2 <- evalExpr expr2
  add val1 val2
evalExpr (Subtr expr1 expr2) = do
  val1 <- evalExpr expr1
  val2 <- evalExpr expr2
  subt val1 val2
evalExpr (Product expr1 expr2) = do
  val1 <- evalExpr expr1
  val2 <- evalExpr expr2
  mul val1 val2

-- TODO: those [g|s]etters are similar, merge them somehow? Probably would require TemplateHaskell tho...

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

doInterpret :: Command -> Shell [Action]
-- TODO: if this has access to IO, then could it not just perform the relevant actions?
-- For now the actions are left in just in case we want to do something in another place.
doInterpret (GenericCmd "pwd" _) = pure . APrint <$> getPath
doInterpret (GenericCmd "cd" (dir:_)) = do
  path <- getPath
  absPath <- liftIO $ withCurrentDirectory path $ canonicalizePath $ T.unpack dir
  ifM (liftIO $ doesDirectoryExist absPath)
    (setPath absPath)
    (liftIO $ TIO.putStrLn $ "Error: no such directory: " `T.append` dir)
  return []
doInterpret (GenericCmd "exit" []) = return [AExit ExitSuccess]
doInterpret (GenericCmd "exit" (code:_)) = case TR.decimal code of
  (Right (exitCode, _)) ->
    if exitCode == 0 then return [AExit ExitSuccess] else return [AExit $ ExitFailure exitCode]
  (Left str) -> do
    liftIO $ TIO.putStrLn $ "Error: wrong exit code: " `T.append` code
    return []
doInterpret (DeclCmd var expr) = do
  either (return $ liftIO $ putStrLn exprError) (setVar var) (runExcept $ evalExpr expr)
  return []
  where
  exprError = "Used wrong expression to declare a variable."
doInterpret (GenericCmd name args) = do
  path <- getPath
  ec <- liftIO $ catch (withCurrentDirectory path $ runProcess (proc name $ map T.unpack args))
    (\e -> do
              let err = show (e :: IOException)
              putStrLn ("Couldn't execute the command with exception: " ++ err)
              return (ExitFailure 666)
              )
  setErrCode ec
  return []
doInterpret _ = return [] -- TODO: more commands :P


interpretCmd :: String -> Shell [Action]
interpretCmd s = do
  liftIO $ putStrLn s
  s' <- doPreprocess s
  liftIO $ putStrLn s'
  cmd <- parseCmd s'
--  liftIO $ putStrLn $ "Interpreting command " ++ show cmd ++ "..."
  doInterpret cmd

handleEvent :: EventResult -> Shell [Action]
handleEvent Nothing = return [AExit ExitSuccess]
handleEvent (Just s) = interpretCmd s


execAction :: MonadIO m => Action -> m ()
execAction = liftIO . go
  where
    go :: Action -> IO ()
    go (APrint s) = putStrLn s
    go (AExit code) = exitWith code

eventsManager :: EventList -> Shell ()
eventsManager [] = return ()
eventsManager events = do
  (event, res) <- waitAny events
  handleEvent res >>= mapM_ execAction
  eventsManager $ delete event events


rgb :: Word8 -> Word8 -> Word8 -> String
rgb r g b = setSGRCode [SetRGBColor Foreground (sRGB24 r g b)]

prompt :: Path -> String
prompt path =
  rgb 72 52 101 ++ "λ " ++ rgb 102 73 142 ++ path ++ rgb 155 62 144 ++ " >>= " ++ setSGRCode [SetDefaultColor Foreground]

startShell :: IO ()
startShell = defaultRunShell loop
  where
    loop :: Shell ()
    loop = do
      path <- getPath
      input <- lift $ async $ getInputLine $ prompt path
      eventsManager [input]
      loop

defaultRunShell :: Shell a -> IO a
defaultRunShell m = do
  st <- initState
  runShell st m

runShell :: ShellState -> Shell a -> IO a
runShell st m = do
  stRef <- newIORef st
  runInputT defaultSettings (runReaderT m stRef)

initState :: IO ShellState
initState = do
  env <- getEnvironment
  ShellState (Map.map VStr (T.pack <$> Map.fromList env)) <$> getCurrentDirectory <*> pure ExitSuccess


hshMain :: IO ()
hshMain = do
  args <- getArgs
  env <- initState
  case args of
    "-c":rest -> runShell env $ interpretCmd (unwords rest) >>= mapM_ execAction
    _ -> startShell

