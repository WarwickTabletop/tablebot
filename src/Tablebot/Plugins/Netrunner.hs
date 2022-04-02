-- |
-- Module      : Tablebot.Plugins.Netrunner
-- Description : A plugin for finding Netrunner cards from Discord.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Commands for interfacing with NetrunnerDB.
module Tablebot.Plugins.Netrunner (netrunner) where

import Tablebot.Plugins.Netrunner.Plugin (netrunnerPlugin)
import Tablebot.Utility (CompiledPlugin, compilePlugin)

netrunner :: CompiledPlugin
netrunner = compilePlugin netrunnerPlugin
