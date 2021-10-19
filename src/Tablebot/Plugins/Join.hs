-- |
-- Module      : Tablebot.Plugins.Join
-- Description : A command that adds a string between each word from an input.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A command that outputs its input.
module Tablebot.Plugins.Join (joinPlugin) where

import Data.Text (Text, pack, intercalate, splitOn)
import Discord.Types
import Tablebot.Plugin
import Tablebot.Plugin.Discord (Message, sendMessage)
import Tablebot.Plugin.Parser (untilEnd)
import Tablebot.Plugin.SmartCommand (parseComm, RestOfInput(ROI), Quoted(Qu))
import Text.RawString.QQ

-- | @join@ outputs the second input interspersed with the first.
join :: Command
join = Command "join" (parseComm joinWithString)
    where joinWithString :: Quoted Text -> RestOfInput Text -> Message -> DatabaseDiscord ()
          joinWithString (Qu toAdd) (ROI t) m =
              sendMessage m 
              $ intercalate toAdd
              $ splitOn " " t

joinHelp :: HelpPage
joinHelp = 
    HelpPage
        "join"
        "Intersperse a message with something"
        [r|**Join**
Repeats the input with some interspersed string or emoji.

*Usage:* `join ":clap:" This text will be interspersed with claps!`|]
    []
    None

-- | @joinPlugin@ assembles the join plugin.
joinPlugin :: Plugin
joinPlugin = plug {commands = [join], helpPages=[joinHelp]}
