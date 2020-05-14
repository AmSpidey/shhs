{-# LANGUAGE OverloadedStrings #-}
module Parser (parseCmd) where

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
doParseLine t = do -- ugh this is ugly
  ecpeb <- runParserT commandParser "input command" t
  (\f -> either f return ecpeb) $ \peb -> do
      liftIO $ putStrLn $ errorBundlePretty peb
      return DoNothing


sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

stringLiteral :: Parser Text
stringLiteral = T.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

pName :: Parser String
pName = lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

notSpace :: Parser Text
notSpace = takeWhile1P Nothing (not . isSpace)

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
