module Builtins where

import Data.Word (Word8)
import Data.Map (Map, fromList)

builtinNames :: [String]
builtinNames =
  ["cd", "pwd", "exit"]

type RGB = (Word8, Word8, Word8)

builtinColors :: Map String RGB
builtinColors =
    fromList [
        ("black", (0,0,0)),
        ("white", (255,255,255)),
        ("red", (255,0,0)),
        ("lime", (0,255,0)),
        ("blue", (0,0,255)),
        ("yellow", (255,255,0)),
        ("cyan", (0,255,255)),
        ("magenta", (255,0,255)),
        ("silver", (192,192,192)),
        ("gray", (128,128,128)),
        ("maroon", (128,0,0)),
        ("olive", (128,128,0)),
        ("green", (0,128,0)),
        ("purple", (128,0,128)),
        ("teal", (0,128,128)),
        ("navy", (0,0,128))
    ]

errColorKey :: String
errColorKey = "ERRCOLOR"

