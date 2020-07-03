{-# LANGUAGE GADTs, DataKinds, FlexibleInstances #-}
module Abs where

import Control.Monad.Reader

import Data.Colour.SRGB hiding (RGB)
import Data.IORef
import Data.Map (Map)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Data.Word

import System.Console.ANSI
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
  show (VStr t) = T.unpack t
  show (VInt i) = show i

type Path = String -- TODO: this should be an actual type

type Env = Map String Val
type AliasMap = Map String String

data HsHandle
  = HHandle Handle
  | HPath Path
  | HPipe

instance Show HsHandle where
  show (HHandle h) = yellow ++ show h ++ resetCol
  show (HPath p) = yellow ++ p ++ resetCol
  show HPipe = yellow ++ "pipe" ++ resetCol

data PIOConfig
  = PIOConfig
  { stdinH :: HsHandle
  , stdoutH :: HsHandle
  , stderrH :: HsHandle
  }

data ShellState
  = ShellState
  { shellStEnv :: Env
  , shellStPath :: Path
  , shellLastErrCode :: ExitCode
  , shellAliases :: AliasMap
  , processConfig :: PIOConfig
  , activeJob :: Maybe Job
  , activeProcesses :: Map Job [Async ExitCode]
  }

data ProcUtils = ProcUtils {puWait :: Shell ExitCode, puCheck :: Shell (Maybe ExitCode)}

type ShellT = ReaderT (IORef ShellState)
type Shell = ShellT (InputT IO)

type EventResult = Maybe String

type EventList = [Async EventResult]

data ProcDesc
  = RunCommand [Text] -- invoked by hsh 'run' command
  | ProgramExec String [Text] -- regular command passed through to the OS
  deriving Show


data RedirectType = RStdin | RStdout | RStderr

instance Show RedirectType where
  show RStdin = "<"
  show RStdout = ">"
  show RStderr = "2>"

type PipelineIO = Shell ()
type PipelineProc = ProcDesc

data PipelineRedirect
  = PRStdoutSink HsHandle
  | PRStderrSink HsHandle
  | PRStdinSource HsHandle
  | PRCancel -- this value means "stop searching for sinks/sources"

instance Show PipelineRedirect where
  show (PRStdoutSink h) = pink ++ "out:" ++ show h
  show (PRStderrSink h) =  pink ++ "err:" ++ show h
  show (PRStdinSource h) =  pink ++ "in:" ++ show h
  show PRCancel =  withColor 255 0 255 "#cancelled" ++ resetCol

-- TODO: refactor this into two ctors PNode and PBound
data Pipeline
  = PRedirect Pipeline PipelineRedirect Pipeline
  | PProc Pipeline PipelineProc Pipeline
  | PAction Pipeline PipelineIO Pipeline
  | PBound


rgb :: Word8 -> Word8 -> Word8 -> String
rgb r g b = setSGRCode [SetRGBColor Foreground (sRGB24 r g b)]

withColor :: Word8 -> Word8 -> Word8 -> String -> String
withColor r g b s = rgb r g b ++ s ++ setSGRCode [SetDefaultColor Foreground]

resetCol :: String
resetCol = setSGRCode [SetDefaultColor Foreground]
red :: String
red = rgb 255 0 0
green = rgb 0 140 0
pink = rgb 255 0 255
yellow = rgb 255 255 0
blue = rgb 0 100 255
instance Show Pipeline where -- only go forward
  show (PRedirect _ rdr next) = red ++ "rdr{" ++ show rdr ++ red ++ "}" ++ resetCol ++ "-" ++ show next
  show (PProc _ desc next) = green ++ "desc{" ++ show desc ++ green ++ "}" ++ resetCol ++ "-" ++ show next
  show (PAction _ _ next) = blue ++ "{IOaction}" ++ resetCol ++ "-" ++ show next
  show PBound = withColor 210 105 30 "|"

showLeft :: Pipeline -> String
showLeft (PRedirect next rdr _) = red ++ "rdr{" ++ show rdr ++ red ++ "}" ++ resetCol ++ "-" ++ showLeft next
showLeft (PProc next desc _) = green ++ "desc{" ++ show desc ++ green ++ "}" ++ resetCol ++ "-" ++ showLeft next
showLeft (PAction next _ _) = blue ++ "{IOaction}" ++ resetCol ++ "-" ++ showLeft next
showLeft PBound = withColor 210 105 30 "|"

doShowLeft :: Pipeline -> String
doShowLeft = showLeft . getLast

getLast :: Pipeline -> Pipeline
getLast p = case p of
  PBound -> PBound
  (PRedirect _ _ n) -> go n
  (PProc _ _ n) -> go n
  (PAction _ _ n) -> go n
  where
    go PBound = p
    go n = getLast n

newtype Job = Job {jobID :: String} deriving (Show, Eq, Ord)

data Action
  = APrintErr String
  | AExit ExitCode
  | ALaunchProc ProcDesc
  | AIOAction (Shell [Action]) -- only for IO actions that don't launch other processes
  | ARedirect RedirectType Path [Action]
  | APipe [[Action]]

data Command
  = DoNothing
  | GenericCmd String [Text]
--  | forall a. TypedCmd String (ParseGuide a)
  | DeclCmd String Expr
  | AliasCmd String String
  | Pipe [Command]
  | RedirectOut Path Command
  | RedirectErr Path Command
  | RedirectIn Path Command

instance Show Action where
  show (APrintErr s) = "printErr(\"" ++ s ++ "\")"
  show (AExit ec) = "exit(" ++ show ec ++ ")"
  show (ALaunchProc desc) = "exec{" ++ show desc ++ "}"
  show (AIOAction _) = "{Some IO action.}"
  show (ARedirect rt p as) = "{" ++ show as ++ "} " ++ show rt ++ " " ++ show p
  show (APipe as) = "pipeline{" ++ intercalate " | " (map show as) ++ "}"

instance Show Command where
  show DoNothing = "DoNothing"
  show (GenericCmd name args) = "cmd{" ++ name ++ "}(" ++ intercalate ", " (map T.unpack args) ++ ")"
  show (DeclCmd str e) = "let " ++ str ++ " = " ++ show e
  show (AliasCmd a v) = "let alias " ++ a ++ " = " ++ v
  show (Pipe l) = intercalate " | " $ map show l
  show (RedirectOut path cmd) = show cmd ++ " > " ++ show path
  show (RedirectErr path cmd) = show cmd ++ " 2> " ++ show path
  show (RedirectIn path cmd) = show cmd ++ " < " ++ show path
  show _ = error "cannot show :("

-- -- | Command type building blocks.
-- -- Designed so that a Megaparsec parser made from them is well-typed.
-- data ParseGuide a where
--   NoArgs   :: ParseGuide ()
--   Many     :: ParseGuide a -> ParseGuide [a]
--   (:+:)    :: ParseGuide a -> ParseGuide b -> ParseGuide (a, b)
--   (:>:)    :: ParseGuide a -> ParseGuide b -> ParseGuide b
--   ExactStr :: String -> ParseGuide Text
--   Discard  :: ParseGuide a -> ParseGuide ()
--   AnyStr   :: ParseGuide Text
--   AnyInt   :: ParseGuide Integer


-- Parser types

type HshParsec s = ParsecT Void s Shell

type Parser = HshParsec Text

type ParserS = HshParsec String

type Err = String


class Defaultable d where
  def :: d

instance Defaultable PIOConfig where
  def = PIOConfig
    { stdinH = HHandle stdin
    , stdoutH = HHandle stdout
    , stderrH = HHandle stderr
    }
