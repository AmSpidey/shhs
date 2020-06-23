{-# LANGUAGE OverloadedStrings, GADTs #-}
module Parser (parseCmd, doPreprocess) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Combinators.Expr
import Data.Char
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Abs
import Builtins
import Utils

-- | Preprocessing part.

doPreprocess :: String -> Shell String
doPreprocess t = do
  ecpeb <- runParserT (linePreprocessor 0) "preprocessing" $ T.pack t
  either (\peb -> t <$ liftIO (putStrLn $ errorBundlePretty peb)) return ecpeb

linePreprocessor :: Integer -> Parser String
linePreprocessor acc = do
  x <- optional pCharAndEscape
  case x of
    Nothing -> return ""
    Just x' -> do
      let escape = x' == '\\'
      let acc' = if escape then acc + 1 else 0
      if x' == '$' && acc `mod` 2 == 0
        then do
          val <- preprocessVar  
          next <- linePreprocessor acc'
          return $ val ++ next
        else do
          next <- linePreprocessor acc'
          return $ (if escape && acc' `mod` 2 == 1 then "" else [x']) ++ next

preprocessVar :: Parser String
preprocessVar = do
    var <- pName
    val <- getVar var
    return $ show $ fromMaybe (VStr "") val

pCharAndEscape :: Parser Char
pCharAndEscape = try (char '\\') <|> L.charLiteral    

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

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.decimal

stringLiteral :: Parser Text
stringLiteral = T.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

pName :: Parser String
pName = lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "command name")

notSpace :: Parser Text
notSpace = lexeme $ takeWhile1P Nothing (not . isSpace)

genericArgument :: Parser Text
genericArgument = stringLiteral <|> notSpace

commandParser :: Parser Command
commandParser = noCommand <|> pCommand

noCommand :: Parser Command
noCommand = DoNothing <$ eof

pLet :: Parser Command
pLet = do
  var <- pName
  symbol "="
  DeclCmd var <$> pExpr


pCommand :: Parser Command
pCommand = do
  name <- pName
  if name == "let"
    then pLet
    else
      if name `elem` builtinNames
        then GenericCmd name <$> many genericArgument
        else GenericCmd name <$> many genericArgument

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
