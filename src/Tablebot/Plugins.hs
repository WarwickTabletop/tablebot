-- |
-- Module      : Tablebot.Plugins
-- Description : Available plugins for Tablebot.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Here is a collection of existing plugins for Tablebot. If you add new plugins
-- to the Plugins directory, include an import here. This means that users only
-- need to import @Tablebot.Plugins@ to import individual plugins.
module Tablebot.Plugins where

import Control.Concurrent.MVar (MVar)
import Data.Text (Text)
import Tablebot.Internal.Administration (ShutdownReason)
import Tablebot.Internal.Plugins (compilePlugin)
import Tablebot.Internal.Types (CompiledPlugin (..))
import Tablebot.Plugins.Administration (administrationPlugin)
import Tablebot.Plugins.Basic (basic)
import Tablebot.Plugins.Cats (cat)
import Tablebot.Plugins.Dogs (dog)
import Tablebot.Plugins.Flip (flips)
import Tablebot.Plugins.Fox (fox)
import Tablebot.Plugins.Netrunner (netrunner)
import Tablebot.Plugins.Ping (pingpong)
import Tablebot.Plugins.Quote (quotes)
import Tablebot.Plugins.Reminder (reminder)
import Tablebot.Plugins.Roll (roll)
import Tablebot.Plugins.Say (says)
import Tablebot.Plugins.Shibe (shibe)
import Tablebot.Plugins.Suggest (suggests)
import Tablebot.Plugins.Welcome (welcome)

-- Use long list format to make additions and removals non-conflicting on git PRs
allPlugins :: [CompiledPlugin]
allPlugins =
  [ pingpong,
    basic,
    cat,
    dog,
    shibe,
    flips,
    fox,
    netrunner,
    quotes,
    reminder,
    says,
    suggests,
    roll,
    welcome
  ]

-- | @addAdministrationPlugin@ is needed to allow the administration plugin to be aware of the list of current plugins
addAdministrationPlugin :: MVar ShutdownReason -> [CompiledPlugin] -> [CompiledPlugin]
addAdministrationPlugin rFlag cps = compilePlugin (administrationPlugin rFlag cps) : cps

-- | @plugs `minusPl` names@ removes all plugins with the given names.
minusPl :: [CompiledPlugin] -> [Text] -> [CompiledPlugin]
minusPl = foldr (\n plugs -> filter ((/= n) . compiledName) plugs)
