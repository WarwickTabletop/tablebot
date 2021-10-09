-- |
-- Module      : Tablebot.Plugin.SmartCommand
-- Description : Doing dumb things with types.
-- Copyright   : (c) Finnbar Keating 2021
-- License     : MIT
-- Maintainer  : finnjkeating@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Can we avoid having to do parsing by using types?
-- Let's find out
module Tablebot.Plugin.SmartCommand where

import Control.Applicative (optional)
import Data.Text (Text, pack)
import Discord.Types (Message)
import Tablebot.Plugin.Parser (integer, quoted, untilSpace)
import Tablebot.Plugin.Types (DatabaseDiscord, Parser)
import Text.Megaparsec (MonadParsec (try), many)

-- | marks that a type can be parsed
class Parsable t where
  par :: Parser t

-- | parsing implementation for all integral types
-- Overlappable due to the really flexible head state
instance {-# OVERLAPPABLE #-} (Integral a, Read a) => Parsable a where
  par = integer

-- | parsing implementation for all single words (all characters)
instance Parsable String where
  par = untilSpace

newtype Quoted = Qu Text

-- | stolen from finnbar's implementation
-- get a quoted string
instance Parsable Quoted where
  par = Qu . pack <$> quoted

-- | get a value that might or might not exist
instance (Parsable a) => Parsable (Maybe a) where
  par = optional $ try par

-- | get many instances of a value
instance {-# OVERLAPPABLE #-} (Parsable a) => Parsable [a] where
  par = many par

-- | make the required command parser
class MakeCommand xs where
  parseMK :: xs -> Parser (Message -> DatabaseDiscord ())

-- | make the base case for the command parser
-- Overlapping as it's the most important case
instance {-# OVERLAPPING #-} MakeCommand (Message -> DatabaseDiscord ()) where
  parseMK comm = return comm

-- | make the recursive case for the command parser
-- Overlappable as we prefer the other case
instance {-# OVERLAPPABLE #-} (Parsable x, MakeCommand xs) => MakeCommand (x -> xs) where
  parseMK comm = do
    res <- par -- parse according to x
    parseMK (comm res) -- apply that argument and keep parsing
