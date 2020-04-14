module Shell where

import System.Exit
import Control.Monad.IO.Class
import UnliftIO
import Data.List
import System.Console.Haskeline

type ShellM = IO
type EventResult = Maybe String

type EventList = [Async EventResult]

data Action = APrint String | AExit (Maybe Int) deriving (Show, Eq)

handleEvent :: EventResult -> [Action]
handleEvent Nothing = [AExit Nothing]
handleEvent (Just s) = [APrint $ s ++ s]

execAction :: MonadIO m => Action -> m ()
execAction = liftIO . go
  where
    go (APrint s) = putStrLn s
    go (AExit Nothing) = exitSuccess
    go (AExit (Just code)) = exitWith (ExitFailure code)

eventsManager :: EventList -> InputT IO ()
eventsManager [] = return ()
eventsManager events = do
  (event, res) <- waitAny events
  mapM_ execAction $ handleEvent res
  eventsManager $ delete event events


execShell :: IO ()
execShell = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      input <- async $ getInputLine "hashell$ "
      eventsManager [input]
      loop
