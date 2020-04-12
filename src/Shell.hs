module Shell where

import Control.Monad.IO.Class
import UnliftIO
import Data.List
import System.Console.Haskeline

type ShellM = IO
type EventResult = Maybe String

handleEvent :: EventResult -> IO [Async EventResult]
handleEvent e = undefined


eventsManager :: [Async EventResult] -> IO ()
eventsManager [] = return ()
eventsManager events = do
  (event, res) <- waitAny events
  newEvents <- handleEvent res
  eventsManager (delete event (events ++ newEvents))


execShell :: IO ()
execShell = do
  runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      input <- async $ getInputLine "hashell$ "
      liftIO $ eventsManager [input]
      return ()
      --eventsManager ([input])
