{-# LANGUAGE OverloadedStrings #-}
module RibTest where


import System.Process.Typed
import qualified UnliftIO.Process as P

import UnliftIO

ribTest :: IO ()
ribTest = do
    (hread, hwrite) <- P.createPipe

    a1 <- async $ runProcess (setNewSession True $ setStdout (useHandleClose hwrite) $ proc "ls" [])
    a2 <- async $ do
      putStrLn "startgrep"
--      hClose hwrite
      runProcess (setNewSession True $ setStdin (useHandleClose hread) $ proc "grep" [".hs"]) 
--        hClose hwrite
      putStrLn "fingrep"
      hFlush stdout
    wait a1
    putStrLn "a1"
    hClose hwrite
    wait a2
    hClose hwrite
    hClose hread
    putStrLn "a2"
    return ()
