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

import Data.Text (Text, intercalate, pack, splitOn)
import Discord.Types
import Tablebot.Plugin
import Tablebot.Plugin.Discord (Message, sendMessage)
import Tablebot.Plugin.Parser (untilEnd)
import Tablebot.Plugin.SmartCommand (Quoted (Qu), RestOfInput (ROI), parseComm)
import Text.RawString.QQ

joinify :: Text -> [Text] -> Text
joinify sep text = sep <> (intercalate sep text) <> sep

-- | @join@ outputs the second input interspersed with the first.
join :: Command
join = Command "join" (parseComm joinWithString)
  where
    joinWithString :: RestOfInput Text -> Message -> DatabaseDiscord ()
    joinWithString (ROI t) m =
      sendMessage m $
        (\(sep:text) -> joinify sep text) $
          splitOn " " t

clap :: Command
clap = Command "clap" (parseComm clappytime)
  where
    clappytime :: RestOfInput Text -> Message -> DatabaseDiscord ()
    clappytime (ROI t) m =
      sendMessage m $
        joinify ":clap:" $
          splitOn " " t

joinHelp :: HelpPage
joinHelp =
  HelpPage
    "join"
    "Intersperse a message with something"
    [r|**Join**
Repeats the input with some interspersed string or emoji.

*Usage:* `join :clap: This text will be interspersed with claps!`|]
    []
    None

clapHelp :: HelpPage
clapHelp =
  HelpPage
    "clap"
    "For:clap:All:clap:Your:clap:Clapping:clap:Needs"
    [r|**Clap**
Insert claps into a message.

*Usage:* `clap This text will be interspersed with claps!`|]
    []
    None

-- | @joinPlugin@ assembles the join plugin.
joinPlugin :: Plugin
joinPlugin = plug {commands = [join, clap], helpPages = [joinHelp, clapHelp]}
