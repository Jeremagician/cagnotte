module Cagnotte.Parser
  ( parseInput
  )
where

import Control.Monad (void)
import Text.Megaparsec hiding (label)
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Cagnotte.Types

sc :: Parser ()
sc = hidden . skipMany $ choice [void spaceChar, lineCmt]
  where lineCmt = L.skipLineComment "#"

lexeme :: Parser a -> Parser a
lexeme p = sc *> p <* sc


integer :: Parser Integer
integer = lexeme L.integer

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

wallet :: Parser Wallet
wallet = everybody <|> owner
  where everybody = symbol "*" *> return Everybody
        owner = (lexeme $ some alphaNumChar) >>= \s -> return $ Owner s


money :: Parser Amount
money = do
  euros <- integer
  cents <- symbol "," *> integer <* symbol "€" -- 1,5€ should work
       <|> symbol "€" *> option 0 integer      -- 1€50 will work aswell
  return $ Amount $ toRational (fromIntegral euros + toCents cents)
    where
      toCents :: Integer -> Float
      toCents c = (fromIntegral c) / 100

transaction :: Parser Transaction
transaction = do
  from <- wallet
  symbol "->"
  to   <- wallet
  symbol ":"
  amount <- money
  label <- option Nothing (parens $ (many (alphaNumChar <|> spaceChar)
                                     >>= return . Just))

  return Transaction
    { from = from
    , to   = to
    , amount = amount
    , label = label
    }


parseInput :: Parser [Transaction]
parseInput = some transaction
