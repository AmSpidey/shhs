{-# LANGUAGE OverloadedStrings #-}
module RibTest where


import System.Process.Typed
import qualified UnliftIO.Process as P

import UnliftIO

ribTest :: IO ()
ribTest = do
    (hread, hwrite) <- P.createPipe

    a1 <- async $ runProcess $ setStdout (useHandleClose hwrite) $ proc "ls" []
    a2 <- async $ runProcess $ setStdin (useHandleClose hread) $ proc "grep" [".hs"]
    wait a1
    hClose hwrite
    wait a2
    hClose hread
    return ()
