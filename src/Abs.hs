{-# LANGUAGE GADTs, ExistentialQuantification #-}
module Abs where

import Control.Monad.Reader
import Data.IORef
import Data.Map (Map)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void

import System.Console.Haskeline
import System.Exit

import Text.Megaparsec
import UnliftIO

-- This is the file where we define types to avoid cyclic dependencies.
-- No project file with actual code should be imported from here.

data Expr
  = Var Text
  | ELit Text
  | EInt Integer
  | Negation Expr
  | Sum      Expr Expr
  | Subtr    Expr Expr
  | Product  Expr Expr
  deriving (Eq, Ord, Show)

data Val = VStr Text | VInt Integer
instance Show Val where
  show (VStr t) = show t
  show (VInt i) = show i

type Path = String -- TODO: this should be an actual type

type Env = Map String Val
type AliasMap = Map String String

data ProcessConfig = ProcessConfig { stdinPath :: Maybe Path, stdoutPath :: Maybe Path, stderrPath ::  Maybe Path }

data ShellState
  = ShellState
  { shellStEnv :: Env
  , shellStPath :: Path
  , shellLastErrCode :: ExitCode
  , shellAliases :: AliasMap
  , processConfig :: ProcessConfig
  }

type ShellT = ReaderT (IORef ShellState)
type Shell = ShellT (InputT IO)

type EventResult = Maybe String

type EventList = [Async EventResult]

data Action = APrint String | AExit ExitCode
  deriving (Show, Eq)

data Command
  = DoNothing
  | GenericCmd String [Text]
  | forall a. TypedCmd String (ParseGuide a)
  | DeclCmd String Expr
  | AliasCmd String String
  | Pipe Command Command
  | RedirectOut Path Command
  | RedirectErr Path Command
  | RedirectIn Path Command

instance Show Command where
  show DoNothing = "DoNothing"
  show (GenericCmd name args) = "cmd{" ++ name ++ "}(" ++ intercalate ", " (map T.unpack args) ++ ")"
  show (DeclCmd str e) = "let " ++ str ++ " = " ++ show e
  show (AliasCmd a v) = "let alias " ++ a ++ " = " ++ v
  show _ = error "cannot show :("

-- | Command type building blocks.
-- Designed so that a Megaparsec parser made from them is well-typed.
data ParseGuide a where
  NoArgs   :: ParseGuide ()
  Many     :: ParseGuide a -> ParseGuide [a]
  (:+:)    :: ParseGuide a -> ParseGuide b -> ParseGuide (a, b)
  (:>:)    :: ParseGuide a -> ParseGuide b -> ParseGuide b
  ExactStr :: String -> ParseGuide Text
  Discard  :: ParseGuide a -> ParseGuide ()
  AnyStr   :: ParseGuide Text
  AnyInt   :: ParseGuide Integer


-- Parser types

type Parser = ParsecT Void Text Shell

type Err = String
