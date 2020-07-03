{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Shell where


import System.IO hiding (hClose, withFile)
import System.Environment
import System.Process.Typed
import System.Console.ANSI.Codes
import System.Console.Haskeline

import Data.Colour.SRGB hiding (RGB)
import Data.Word (Word8)
import Data.List
import Data.Text (Text)
import qualified Data.Text.Read as TR
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Map ((!?))

import Control.Monad.Except
import Control.Monad.Reader

import UnliftIO.Directory
import UnliftIO hiding (Handler)

import Abs
import Builtins
import Parser
import Utils
import StateUtils

import System.Exit
import System.Posix.Signals
import Control.Concurrent

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
  withCurrentDirectory path $ canonicalizePath name

printFail :: String -> Shell [Action]
printFail msg = do
  setErrCode (ExitFailure 1)
  return [APrintErr msg]


doInterpret :: Command -> Shell [Action]
-- TODO: if this has access to IO, then could it not just perform the relevant actions?
-- For now the actions are left in just in case we want to do something in another place.
doInterpret (GenericCmd "pwd" _) = pure . APrint <$> getPath
doInterpret (GenericCmd "cd" (dir:_)) = do
  path <- getPath
  absPath <- withCurrentDirectory path $ canonicalizePath $ T.unpack dir
  ifM (doesDirectoryExist absPath)
    (setPath absPath >> return [])
    (printFail $ "Error: no such directory: " ++ T.unpack dir)

doInterpret (GenericCmd "exit" []) = return [AExit ExitSuccess]
doInterpret (GenericCmd "exit" (code:_)) = case TR.decimal code of
  Right (exitCode, _) -> return
    [AExit $ if exitCode == 0 then ExitSuccess else ExitFailure exitCode]
  Left str -> printFail $ "Error: wrong exit code: " ++ str
doInterpret (GenericCmd "run" args) = executeArgs args
doInterpret (DeclCmd var expr) = if var == "EXITCODE" then printFail "Cannot assign to this variable." else
  case runExcept $ evalExpr expr of
    Left msg -> printFail msg
    Right val -> setVar var val >> return []
doInterpret (AliasCmd alias val) =
  if alias == "let"
    then printFail "Cannot alias with the name \"let\"."
    else addAlias alias val >> return []
doInterpret (GenericCmd name args) = runProg name args
doInterpret (Pipe src dst) = do
  (filename, h) <- liftIO $ openTempFile "." "tmp"
  hClose h
  setStdoutPath filename
  act <- doInterpret src
  setStdinPath filename
  act' <- doInterpret dst
  removeFile filename
  return $ act ++ act'
doInterpret (RedirectIn path cmd) = setStdinPath path >> doInterpret cmd
doInterpret (RedirectOut path cmd) = setStdoutPath path >> doInterpret cmd
doInterpret (RedirectErr path cmd) = setStderrPath path >> doInterpret cmd
doInterpret _ = return [] -- TODO: more commands :P

executeArgs :: [Text] -> Shell [Action]
executeArgs args = case args of
  name:args' -> do
    name' <- resolveLocalName $ T.unpack name
    catch (withFile name' ReadMode (\fHandle -> do
        fContents <- liftIO $ hGetContents fHandle
        case fContents of
          '#':'!':_ -> hClose fHandle >> runProg name' args'
          _ -> do
            actionsList <- mapM interpretCmd $ joinByBackslash $ lines fContents
            hClose fHandle
            return $ concat actionsList
      )) printOpenErr
  _ -> printFail "run: Nothing to execute."

printOpenErr :: IOException -> Shell [Action]
printOpenErr e = printFail $ "Could not open file with exception: " ++ show e

runProg :: String -> [Text] -> Shell [Action]
runProg name args = do
  path <- getPath
  config <- getConfig
  (stdinStream, stdinHandle) <- liftIO $ getStream (stdinPath config) ReadMode
  (stdoutStream, stdoutHandle) <- liftIO $ getStream (stdoutPath config) WriteMode
  (stderrStream, stderrHandle) <- liftIO $ getStream (stderrPath config) WriteMode
  ec <- liftIO $
    catch (withCurrentDirectory path $ runProcess $ setStderr stderrStream $ setStdout stdoutStream $ setStdin stdinStream (proc name $ map T.unpack args))
    (\e -> putStrLn ("Couldn't execute the command with exception: " ++ show (e :: IOException)) >> return (ExitFailure 666))
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


execAction :: Action -> Shell ()
execAction (APrint s) = liftIO $ putStrLn s
execAction (APrintErr s) =
  getErrColor >>= \case
    Nothing -> liftIO $ hPutStrLn stderr s
    Just (r, g, b) -> liftIO $ hPutStrLn stderr (withColor r g b s)

  where
    getErrColor :: Shell (Maybe RGB)
    getErrColor = getVar errColorKey >>= \case
      Just (VStr txt) -> return $ builtinColors !? T.unpack txt
      _ -> return Nothing

execAction (AExit code) = liftIO $ exitWith code

eventsManager :: EventList -> Shell ()
eventsManager [] = return ()
eventsManager events = do
  (event, res) <- waitAny events
  handleEvent res >>= mapM_ execAction
  eventsManager $ delete event events


rgb :: Word8 -> Word8 -> Word8 -> String
rgb r g b = setSGRCode [SetRGBColor Foreground (sRGB24 r g b)]

withColor :: Word8 -> Word8 -> Word8 -> String -> String
withColor r g b s = rgb r g b ++ s ++ setSGRCode [SetDefaultColor Foreground]

getPrompt :: Shell String
getPrompt = do
  pr <- getVarStr "PROMPT"
  home <- getVarStr "HOME"
  curDir <- getVarStr "PWD"
  setVar "HOME" (VStr "~")
  case trimCommonPrefix home curDir of
    Nothing -> return ()
    Just suf -> setVar "PWD" (strToVal $ "~" ++ suf)
  res <- doPreprocess pr
  setVar "HOME" (strToVal home)
  setVar "PWD" (strToVal curDir)
  return res

  where
  -- trimCommonPrefix x (x ++ y) = Just y
  -- trimCommonPrefix _ _        = Nothing
  trimCommonPrefix :: String -> String -> Maybe String
  trimCommonPrefix [] ys = Just ys
  trimCommonPrefix (x:xs) (y:ys) | x == y = trimCommonPrefix xs ys
  trimCommonPrefix _ _ = Nothing

runDotFile :: Shell ()
runDotFile = do
  home <- getVarStr "HOME"
  whenM (doesFileExist $ home ++ "/.hshrc") $ void $ interpretCmd "run ~/.hshrc"


startShell :: IO ()
startShell = defaultRunShell $ runDotFile >> loop
  where
    loop :: Shell ()
    loop = do
      path <- getPath
      pr <- getPrompt
      input <- lift $ async $ getInputLine pr
      eventsManager [input]
      loop

defaultRunShell :: Shell a -> IO a
defaultRunShell m = do
  st <- initState
  runShell st m

runShell :: ShellState -> Shell a -> IO a
runShell st m = do
  homeDir <- getHomeDirectory
  stRef <- newIORef st
  let initSettings = (defaultSettings :: Settings IO)
        { historyFile = Just $ homeDir ++ "/.hsh_history"
        , complete = \ss -> do
            st <- readIORef stRef
            withCurrentDirectory (shellStPath st) $ completeFilename ss} :: Settings IO
  runInputT initSettings (runReaderT m stRef)

initEnv :: Env -> IO Env
initEnv env = do
  home <- strToVal <$> getHomeDirectory
  curDir <- strToVal <$> getCurrentDirectory
  let initVars = Map.fromList [("HOME", home), ("PWD", curDir), ("PROMPT", defaultPromptVal)]
  return $ Map.union initVars env

  where
  defaultPromptVal :: Val
  defaultPromptVal = 
    strToVal $ rgb 72 52 101 ++ "λ " ++ rgb 102 73 142 ++ "$PWD" ++ rgb 155 62 144 ++ " >>= " ++ setSGRCode [SetDefaultColor Foreground]


initState :: IO ShellState
initState = do
  env <- Map.map strToVal . Map.fromList <$> getEnvironment
  env' <- initEnv env
  ShellState env'
    <$> getCurrentDirectory
    <*> pure ExitSuccess
    <*> pure Map.empty
    <*> pure emptyConfig

ignoreSIGINT :: IO Handler
ignoreSIGINT = installHandler keyboardSignal (Catch (return ())) Nothing

hshMain :: IO ()
hshMain = do
  args <- getArgs
  env <- initState
  case args of
    "-c":rest -> runShell env $ interpretCmd (unwords rest) >>= mapM_ execAction
    _ -> ignoreSIGINT >> startShell

