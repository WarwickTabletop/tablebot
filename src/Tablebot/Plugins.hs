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
module Tablebot.Plugins
  ( plugins,
  )
where

import Tablebot.Plugin (Plugin)
import Tablebot.Plugins.Basic (basicPlugin)
import Tablebot.Plugins.Cats (catPlugin)
import Tablebot.Plugins.Flip (flipPlugin)
import Tablebot.Plugins.Ping (pingPlugin)
import Tablebot.Plugins.Quote (quotePlugin)
import Tablebot.Plugins.Reminder (reminderPlugin)
import Tablebot.Plugins.Say (sayPlugin)
import Tablebot.Plugins.Join (joinPlugin)
import Tablebot.Plugins.Welcome (welcomePlugin)

-- Use long list format to make additions and removals non-conflicting on git PRs
plugins :: [Plugin]
plugins =
  [ pingPlugin,
    basicPlugin,
    catPlugin,
    flipPlugin,
    quotePlugin,
    reminderPlugin,
    sayPlugin,
    joinPlugin,
    welcomePlugin
  ]
