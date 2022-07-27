{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Tablebot.Utility.SmartParser.Types
-- Description : Some of the types or typeclasses for smart parsers.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Tablebot.Utility.SmartParser.Types where

import Data.Proxy (Proxy (..))
import Data.Text (Text, pack)
import Discord.Types
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Tablebot.Utility.Types
import Text.Megaparsec (observing)

newtype SenderUserId = SenderUserId UserId deriving (Show, Eq)

-- | Custom infix operator to replace the error of a failing parser (regardless
-- of parser position) with a user given error message.
--
-- Has some effects on other error parsing. Use if you want the error you give
-- to be the one that is reported (unless this is used at a higher level.)
--
-- Overwrites/overpowers WithError errors.
(<??>) :: Parser a -> String -> Parser a
(<??>) p s = do
  r <- observing p
  case r of
    Left _ -> fail s
    Right a -> return a

-- | @Quoted a@ defines an input of type @a@ that is contained within quotes.
newtype Quoted a = Qu {quote :: a} deriving (Show)

-- | @RestOfInput a@ parses the rest of the input, giving a value of type @a@.
newtype RestOfInput a = ROI {unROI :: a}

-- | @Exactly s@ defines an input exactly matching @s@ and nothing else.
data Exactly (s :: Symbol) = Ex

-- | @RestOfInput a@ parses the rest of the input, giving a value of type @a@.
newtype RestOfInput1 a = ROI1 a

-- | @WithError err x@ parses an @x@, reporting @err@ if the parsing of @x@
-- fails.
newtype WithError (err :: Symbol) x = WErr x

-- | Labelled value for use with smart commands.
--
-- This is for use with slash commands, where there is a name and description
-- required.
newtype Labelled (name :: Symbol) (desc :: Symbol) a = Labelled {unLabel :: a}

-- | Easily make a labelled value.
labelValue :: forall n d a. a -> Labelled n d a
labelValue = Labelled @n @d

-- | Get the name and description of a labelled value.
getLabelValues :: forall n d a. (KnownSymbol n, KnownSymbol d) => Proxy (Labelled n d a) -> (Text, Text)
getLabelValues _ = (pack (symbolVal (Proxy :: Proxy n)), pack (symbolVal (Proxy :: Proxy d)))
