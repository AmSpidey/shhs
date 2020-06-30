{-# LANGUAGE OverloadedStrings #-}
module Shell where

import System.IO hiding (hClose)
import System.Exit
import System.Environment
import System.Directory
import System.Process.Typed
import System.Console.ANSI.Codes
import System.Console.Haskeline

import Data.Colour.SRGB
import Data.Word (Word8)
import Data.List
import Data.Text (Text)
import qualified Data.Text.Read as TR
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as Map

import Control.Monad.Except
import Control.Monad.Reader
import UnliftIO

import Abs
import Parser
import Utils
import StateUtils

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
evalExpr (ELit s) = return $ VStr s
evalExpr (EInt i) = return $ VInt i
evalExpr (Negation expr) = evalExpr $ Product (EInt (-1)) expr
evalExpr (Sum expr1 expr2) = join $ add <$> evalExpr expr1 <*> evalExpr expr2
evalExpr (Subtr expr1 expr2) = join $ subt <$> evalExpr expr1 <*> evalExpr expr2
evalExpr (Product expr1 expr2) = join $ mul <$> evalExpr expr1 <*> evalExpr expr2
evalExpr _ = throwError "Undefined type of expression"

getStream :: Maybe Path -> IOMode -> IO (StreamSpec s (), Maybe Handle)
getStream Nothing _ = return (inherit, Nothing)
getStream (Just path) mode = do
  handle' <- openFile path mode
  return (useHandleClose handle', Just handle')

closeHandle :: Maybe Handle -> IO ()
closeHandle Nothing = return ()
closeHandle (Just h) = hClose h

resolveLocalName :: String -> Shell String
resolveLocalName name = do
  path <- getPath
  liftIO $ withCurrentDirectory path $ canonicalizePath name

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
  Right (exitCode, _) -> return
    [AExit $ if exitCode == 0 then ExitSuccess else ExitFailure exitCode]
  Left str -> return [APrint $ "Error: wrong exit code: " ++ str]
doInterpret (GenericCmd "run" args) = case args of
  name:args' -> do
    name' <- resolveLocalName $ T.unpack name
    runProg name' args'
  _ -> return [APrint "run: Nothing to execute."]
doInterpret (DeclCmd var expr) = either (return $ liftIO $ putStrLn exprError) (setVar var) (runExcept $ evalExpr expr)
  >> return []
  where
  exprError = "Used wrong expression to declare a variable."
doInterpret (AliasCmd alias val) =
  if alias == "let"
    then setErrCode (ExitFailure 1) >> return [APrint "Cannot alias with the name \"let\"."]
    else addAlias alias val >> return []
doInterpret (GenericCmd name args) = runProg name args
doInterpret (Pipe src dst) = do
  (filename, h) <- liftIO $ openTempFile "." "tmp"
  liftIO $ hClose h
  setStdoutPath filename
  act <- doInterpret src
  setStdinPath filename
  act' <- doInterpret dst
  liftIO $ removeFile filename
  return $ act ++ act'
doInterpret (RedirectIn path cmd) = setStdinPath path >> doInterpret cmd
doInterpret (RedirectOut path cmd) = setStdoutPath path >> doInterpret cmd
doInterpret (RedirectErr path cmd) = setStderrPath path >> doInterpret cmd
doInterpret _ = return [] -- TODO: more commands :P

runProg :: String -> [Text] -> Shell [Action]
runProg name args = do
  path <- getPath
  config <- getConfig
  (stdinStream, stdinHandle) <- liftIO $ getStream (stdinPath config) ReadMode
  (stdoutStream, stdoutHandle) <- liftIO $ getStream (stdoutPath config) WriteMode
  (stderrStream, stderrHandle) <- liftIO $ getStream (stderrPath config) WriteMode
  ec <- liftIO $ catch (withCurrentDirectory path $ runProcess $ setStderr stderrStream $ setStdout stdoutStream $ setStdin stdinStream (proc name $ map T.unpack args))
    (\e -> putStrLn ("Couldn't execute the command with exception: " ++ show (e :: IOException))
           >> return (ExitFailure 666))
  liftIO $ closeHandle stdinHandle
  liftIO $ closeHandle stdoutHandle
  liftIO $ closeHandle stderrHandle
  setConfig $ const emptyConfig
  setErrCode ec
  return []

interpretCmd :: String -> Shell [Action]
interpretCmd s = doPreprocess s >>= parseCmd >>= doInterpret
  -- liftIO $ putStrLn $ "Original command: \"" ++ s ++ "\""
  -- s' <- doPreprocess s
  -- liftIO $ putStrLn $ "After preprocess: \"" ++ s' ++ "\""
  -- cmd <- parseCmd s'
  -- liftIO $ putStrLn $ "Parsed command to " ++ show cmd ++ ", interpreting..."
  -- doInterpret cmd

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

initEnv :: Env -> IO Env
initEnv env = do
  home <- VStr . T.pack <$> getHomeDirectory
  return $ Map.insert "HOME" home env


initState :: IO ShellState
initState = do
  env <- Map.map (VStr . T.pack) . Map.fromList <$> getEnvironment
  env' <- initEnv env
  ShellState env'
    <$> getCurrentDirectory
    <*> pure ExitSuccess
    <*> pure Map.empty
    <*> pure emptyConfig


hshMain :: IO ()
hshMain = do
  args <- getArgs
  env <- initState
  case args of
    "-c":rest -> runShell env $ interpretCmd (unwords rest) >>= mapM_ execAction
    _ -> startShell

