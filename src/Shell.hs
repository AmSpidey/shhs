module Shell where

import Control.Concurrent
import Control.Concurrent.Async
import Data.List

type ShellM = IO
type EventResult = Char

handleEvent :: EventResult -> IO ([Async EventResult])
handleEvent e = undefined


eventsManager :: [Async EventResult] -> IO ()
eventsManager [] = return ()
eventsManager events = do
  (event, res) <- waitAny events
  newEvents <- handleEvent res
  eventsManager (delete event (events ++ newEvents))


execShell :: IO ()
execShell = do
  char <- async getChar
  eventsManager ([char])
  execShell
