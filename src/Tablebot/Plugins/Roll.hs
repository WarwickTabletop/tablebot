-- |
-- Module      : Tablebot.Plugins.Roll
-- Description : A command that outputs the result of rolling dice.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A command that outputs the result of rolling the input dice.
module Tablebot.Plugins.Roll (roll) where

import Tablebot.Plugins.Roll.Plugin (rollPlugin)
import Tablebot.Utility

roll :: CompiledPlugin
roll = compilePlugin rollPlugin
