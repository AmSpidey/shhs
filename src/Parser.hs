{-# LANGUAGE OverloadedStrings, GADTs #-}
module Parser (parseCmd) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import qualified Data.Text as T
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Abs
import Builtins

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

pCommand :: Parser Command
pCommand = do
  name <- pName
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
