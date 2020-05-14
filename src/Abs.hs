module Abs where

import Control.Monad.Reader
import Data.IORef
import Data.Map (Map)
import Data.Text (Text)
import Data.Void

import System.Console.Haskeline
import Text.Megaparsec
import UnliftIO

-- This is the file where we define types to avoid cyclic dependencies.
-- No project file with actual code should be imported from here.


data Val = VStr String

type Path = String -- TODO: this should be an actual type

type Env = Map String Val

data ShellState = ShellState { shellStEnv :: Env, shellStPath :: Path }

type ShellT = ReaderT (IORef ShellState)
type Shell = ShellT (InputT IO)

type EventResult = Maybe String

type EventList = [Async EventResult]

data Action = APrint String | AExit (Maybe Int)
  deriving (Show, Eq)

data Command
  = DoNothing
  | GenericCmd String [Text]
  | TypedCmd String -- TODO type
  deriving (Show)

-- Parser types

type Parser = ParsecT Void Text Shell
