{-# LANGUAGE OverloadedStrings #-}
module RibTest where


import System.Process.Typed
import qualified UnliftIO.Process as P

import UnliftIO

ribTest :: IO ()
ribTest = do
    (hread, hwrite) <- P.createPipe

    a1 <- asyncBound $ do
      runProcess (setNewSession True $ setCloseFds True $ setStdout (useHandleClose hwrite) $ proc "ls" [])
    a2 <- asyncBound $ do
--      hClose hwrite
      runProcess (setNewSession True $ setCloseFds True $ setStdin (useHandleClose hread) $ proc "grep" [".hs"]) 
--        hClose hwrite
    wait a1
    putStrLn "a1"
    hClose hwrite
    wait a2
    
    hClose hwrite
    hClose hread
    putStrLn "a2"
    return ()
