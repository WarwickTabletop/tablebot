-- |
-- Module      : Tablebot.Utility.SmartParser
-- Description : Automatic parser generation from function types.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Generates a parser based on the shape of the command function.
-- For example, if you have a command that takes in an Int as argument, we
-- build a parser that reads in that Int and then runs the command.
module Tablebot.Utility.SmartParser
  ( module Tablebot.Utility.SmartParser.SmartParser,
    module Tablebot.Utility.SmartParser.Interactions,
    module Tablebot.Utility.SmartParser.Types,
  )
where

import Tablebot.Utility.SmartParser.Interactions
import Tablebot.Utility.SmartParser.SmartParser
import Tablebot.Utility.SmartParser.Types
