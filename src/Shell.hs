module Shell where

import System.Exit
import System.Environment
import System.Directory
import Control.Monad.IO.Class
import UnliftIO
import Data.List
import System.Console.Haskeline
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map

data Val = VStr String

type Path = String -- TODO: this should be an actual type

type Env = Map String Val

data ShellState = ShellState { shellStEnv :: Env, shellStPath :: Path }

type ShellT = ReaderT ShellState
type Shell = ShellT (InputT IO)

type EventResult = Maybe String

type EventList = [Async EventResult]

data Action = APrint String | AExit (Maybe Int) deriving (Show, Eq)

data Command = TmpCmd String [String] -- TODO: more sophisticated data type

parseCmd :: String -> Shell Command
parseCmd s = return $
  case words s of
    [] -> TmpCmd "" []
    x : xs -> TmpCmd x xs -- TODO: escape the string

getPath :: Shell Path
getPath = asks shellStPath


doInterpret :: Command -> Shell [Action]
doInterpret (TmpCmd "pwd" []) = (:[]) . APrint <$> getPath
doInterpret _ = return [] -- TODO: more commands :P


interpretCmd :: String -> Shell [Action]
interpretCmd s = do
  cmd <- parseCmd s
  doInterpret cmd

handleEvent :: EventResult -> Shell [Action]
handleEvent Nothing = return [AExit Nothing]
handleEvent (Just s) = interpretCmd s

execAction :: MonadIO m => Action -> m ()
execAction = liftIO . go
  where
    go (APrint s) = putStrLn s
    go (AExit Nothing) = exitSuccess
    go (AExit (Just code)) = exitWith (ExitFailure code)

eventsManager :: EventList -> Shell ()
eventsManager [] = return ()
eventsManager events = do
  (event, res) <- waitAny events
  handleEvent res >>= mapM_ execAction
  eventsManager $ delete event events


startShell :: IO ()
startShell = defaultRunShell loop
  where
    loop :: Shell ()
    loop = do
      input <- lift $ async $ getInputLine "hashell$ "
      eventsManager [input]
      loop

defaultRunShell :: Shell a -> IO a
defaultRunShell m = do
  st <- initState
  runInputT defaultSettings (runReaderT m st)

runShell :: ShellState -> Shell a -> IO a
runShell st m = runInputT defaultSettings (runReaderT m st)

initState :: IO ShellState
initState = do
  env <- getEnvironment
  ShellState (Map.map VStr $ Map.fromList env) <$> getCurrentDirectory


hshMain :: IO ()
hshMain = do
  args <- getArgs
  env <- initState
  case args of
    "-c":rest -> runShell env $ interpretCmd (unwords rest) >>= mapM_ execAction
    _ -> startShell

