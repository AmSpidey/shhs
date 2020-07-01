{-# LANGUAGE OverloadedStrings, GADTs, RecordWildCards #-}
module Parser (parseCmd, doPreprocess) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Combinators.Expr
import Data.Char
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Abs
import Utils
import StateUtils


-- | Type used for parsing command arguments

data Arg = Generic Text | Redirect (Text, Path)

-- | Preprocessing part.
--this is my best attempt at a bitmap one could pattern match into

data EscapeState = ENormal | EEscaped
data SpaceState = ENoSpace | EAfterSpace
data StrState = ENoStr | EString

data EscapingState = EState
  { escSt :: EscapeState
  , spSt :: SpaceState
  , strSt :: StrState
  }

class Defaultable d where
  def :: d

instance Defaultable EscapeState where
  def = ENormal

instance Defaultable SpaceState where
  def = ENoSpace

instance Defaultable StrState where
  def = ENoStr

instance Defaultable EscapingState where
  def = EState def def def

class Flippable f where
  flup :: f -> f

instance Flippable StrState where
  flup ENoStr = EString
  flup EString = ENoStr

unescaper :: String -> Shell String
unescaper = go def
  where
    go :: EscapingState -> String -> Shell String
    go _ [] = return []
    go e@EState{escSt = ENormal} ('\\':rest) = go e{escSt = EEscaped} rest
    go e@EState{escSt = ENormal} ('$':rest) = do
      let rep = def{strSt = strSt e}
      ecpeb <- runParserT pVarNameAndRest "preprocessing" rest -- TODO: this makes preprocessing O(n^2)
      case ecpeb of
        Left peb -> --do liftIO $ putStrLn $ "Getting variable name failed!\n" ++ errorBundlePretty peb
                       ('$':) <$> go rep rest
        Right (name, rest') -> do
          val <- getVarStr name
          (val ++) <$> go rep rest'
    go e@EState{escSt = EEscaped, strSt = ENoStr} (' ':rest) = ('\\':) . (' ':) <$> go e{escSt = def} rest
    go e@EState{escSt = ENormal} (' ':rest) = (' ':) <$> go e{spSt = EAfterSpace} rest
    go EState{escSt = ENormal, spSt = EAfterSpace, strSt = ENoStr} ('~':rest) = do
      home <- getVarStr "HOME"
      (home ++) <$> go def rest
    go e@EState{escSt = EEscaped} (c:rest) = ('\\':) . (c:) <$> go e{escSt = def} rest
    go e ('\"':rest) = ('\"':) <$> go e{strSt = flup $ strSt e} rest
    go e ('\'':rest) = ('\'':) <$> go e{strSt = flup $ strSt e} rest
    go e (c:rest) = (c:) <$> go def{strSt = strSt e} rest

doPreprocess :: String -> Shell String
doPreprocess = unescaper


pVarNameAndRest :: ParserS (String, String)
pVarNameAndRest = (,) <$> pNameS <*> takeWhileP Nothing (const True)


-- | Parsing expressions.

pInteger :: Parser Expr
pInteger = EInt <$> integer

pLit :: Parser Expr
pLit = ELit <$> stringLiteral

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser Expr
pTerm = choice
  [ parens pExpr
  , pLit
  , pInteger
  ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [
    [ prefix "-" Negation
    , prefix "+" id
    ]
  , [ binary "*" Product
    ]
  , [ binary "+" Sum
    , binary "-" Subtr
    ]
  ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

prefix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)

-- | Parsing utilities.
-- Parsing is done on Text instead of String to improve performance when parsing source code files.

parseCmd :: String -> Shell Command
parseCmd = doParseLine . T.pack

doParseLine :: Text -> Shell Command
doParseLine t = do
  ecpeb <- runParserT commandParser "input command" t
  either (\peb -> DoNothing <$ liftIO (putStrLn $ errorBundlePretty peb)) return ecpeb

-- TODO: maybe one can make those for generic Char streams?

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

scS :: ParserS ()
scS = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lexemeS :: ParserS a -> ParserS a
lexemeS = L.lexeme scS

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.decimal

-- XXX: debug
debPrInput :: Parser ()
debPrInput = do
  inp <- getInput
  liftIO $ putStrLn $ "remaining input: " ++ T.unpack inp

quote :: Parser Char
quote = char '\"' <|> char '\''

stringLiteral :: Parser Text
stringLiteral = lexeme $ quote *> takeWhileP Nothing (/= '\"') <* quote

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)

pName :: Parser String
pName = lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "command name")

pNameS :: ParserS String
pNameS = lexemeS ((:) <$> letterChar <*> many alphaNumChar <?> "command name")

notSpaceOrPipe :: Parser Text
notSpaceOrPipe = lexeme $ takeWhile1P Nothing $ (not . isSpace) .&& (/= '|')

genericArgument :: Parser Text
genericArgument = stringLiteral <|> notSpaceOrPipe

genericArg :: Parser Arg
genericArg = Generic <$> genericArgument

redirectArg :: Parser Arg
redirectArg = do
  opName <- choice $ symbol <$> [ "<", ">", "2>" ]
  path <- T.unpack <$> genericArgument
  return $ Redirect (opName, path)

distributeArgs :: [Arg] -> ([Text], [(Text, Path)])
distributeArgs (Redirect (op, path):t) = (genericArgs, (op, path):redirectArgs)
  where
    (genericArgs, redirectArgs) = distributeArgs t
distributeArgs (Generic arg:t) = (arg:genericArgs, redirectArgs)
  where
    (genericArgs, redirectArgs) = distributeArgs t
distributeArgs [] = ([], [])

constructCommand :: String -> [(Text, Path)] -> [Text] -> Command
constructCommand name [] args = GenericCmd name args
constructCommand name (("<", path):t) args = RedirectIn path $ constructCommand name t args
constructCommand name ((">", path):t) args = RedirectOut path $ constructCommand name t args
constructCommand name (("2>", path):t) args = RedirectErr path $ constructCommand name t args


commandParser :: Parser Command
commandParser = sc >> noCommand <|> pCommandList

noCommand :: Parser Command
noCommand = DoNothing <$ eof

pLet :: Parser Command
pLet = pLetAlias <|> pLetVar

pLetAlias :: Parser Command
pLetAlias =
  AliasCmd <$ pKeyword "alias" <*> pName <* symbol "=" <*> (T.unpack <$> genericArgument)

pLetVar :: Parser Command
pLetVar = do
  var <- pName
  symbol "="
  DeclCmd var <$> pExpr


addPrefix :: String -> Parser ()
addPrefix s = do
  inp <- getInput
-- TODO: this will not do when input is script file, O(n^2) complexity!
  setInput $ T.snoc (T.pack s) ' ' `T.append` inp
--  inp' <- getInput
--  liftIO $ putStrLn $ "new input: \"" ++ T.unpack inp' ++ "\""

addAliasPrefix :: String -> Parser ()
addAliasPrefix s = getAlias s >>= addPrefix

pCommand :: Parser Command
pCommand = go Set.empty
  where
    go :: Set String -> Parser Command
    go m = do
      name <- pName
      if name == "let"
        then pLet
        else ifM ((name `Set.notMember` m &&) <$> isAlias name) (addAliasPrefix name >> go (Set.insert name m)) $ do
          (genericArgs, redirectArgs) <- distributeArgs <$> many (redirectArg <|> genericArg)
          return $ constructCommand name redirectArgs genericArgs

pCommandList :: Parser Command
pCommandList = foldl1 Pipe <$> pCommand `sepBy1` symbol "|"

genericCommandGuide :: ParseGuide [Text]
genericCommandGuide = Many AnyStr

parserFromGuide :: ParseGuide a -> Parser a
parserFromGuide = go
  where
    go :: ParseGuide a -> Parser a
    go NoArgs = return ()
    go (Many l) = many (go l)
    go (a :+: b) = (,) <$> go a <*> go b
    go (a :>: b) = go a >> go b
    go (ExactStr s) = symbol (T.pack s)
    go (Discard a) = void (go a)
    go AnyStr = genericArgument
    go AnyInt = integer
