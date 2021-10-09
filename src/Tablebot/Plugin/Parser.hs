-- |
-- Module      : Tablebot.Plugin.Parser
-- Description : Helpful parsers for building plugins.
-- Copyright   : (c) Finnbar Keating 2021
-- License     : MIT
-- Maintainer  : finnjkeating@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains helpful parsers for building plugins, along with a few
-- reexports from "Text.Parsec" to avoid having to import it.
module Tablebot.Plugin.Parser where

-- TODO: Much helpful functionality is missing here.

import Data.Char (isDigit, isLetter, isSpace)
import Data.Functor (($>))
import Tablebot.Plugin (Parser)
import Text.Megaparsec

space :: Parser ()
space = satisfy isSpace $> ()

digit :: Parser Char
digit = satisfy isDigit

letter :: Parser Char
letter = satisfy isLetter

givenChar :: Char -> Parser Char
givenChar = satisfy . (==)

-- | @skipSpace@ is a parser that skips many space characters.
skipSpace :: Parser ()
skipSpace = skipMany space

-- | @noArguments@ is a parser that only accepts space characters followed by
-- an end-of-file character, and then runs the input function @f@. Useful for
-- building parsers for commands that take no arguments.
noArguments :: a -> Parser a
noArguments f = (skipSpace *> (eof <?> "No arguments were needed!")) $> f

-- | @quoted@ looks for a quoted string - i.e. one of the form @"text"@.
-- It returns the string found within the quotes if successful, or throws a
-- clear error.
quoted :: Parser String
-- TODO: deal with backslash escapes properly.
quoted = quotedWith '"' <|> quotedWith '\''
  where
    quotedWith :: Char -> Parser String
    quotedWith c =
      between
        (single c <?> "Couldn't find opening quote.")
        (single c <?> "Couldn't find closing quote.")
        (some $ anySingleBut c)
        <?> "Couldn't get quote!"

-- | @word@ parses a single word of letters only.
word :: Parser String
word = some letter

-- | @number@ parses any whole, non-negative number.
number :: Parser Int
number = read <$> some digit

-- | @integer@ parses any whole number
integer :: (Integral a, Read a) => Parser a
integer = do
  minus <- option "-" (return " ")
  digits <- some digit
  return (read (head minus : digits))

-- | @untilEnd@ gets all of the characters up to the end of the input.
untilEnd :: Parser String
untilEnd = do
  c <- anySingle
  cs <- manyTill anySingle eof
  return (c : cs)

-- | @untilSpace@ gets all of the characters up to the next space.
untilSpace :: Parser String
untilSpace = do
  c <- anySingle
  cs <- manyTill anySingle (space <|> eof)
  return (c : cs)

-- | @discordUser@ gets a Discord user from its input.
-- This means that it matches @<\@longidhere>@.
discordUser :: Parser String
discordUser = do
  num <- between (chunk "<@") (single '>') (some digit)
  return $ "<@" ++ num ++ ">"

-- | @sp@ parses an optional space character.
sp :: Parser ()
sp = space <|> pure ()