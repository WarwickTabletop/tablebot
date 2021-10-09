-- |
-- Module      : Tablebot.Plugins.Swap
-- Description : A very simple plugin that provides cat pictures.
-- Copyright   : (c) Finnbar Keating 2021
-- License     : MIT
-- Maintainer  : finnjkeating@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This is an example plugin which just responds with a cat photo to a .cat call
module Tablebot.Plugins.Swap where

import Control.Monad (void)
import Data.Text (pack)
import Discord.Types ( Message )
import Tablebot.Plugin
    ( DatabaseDiscord, Plugin(commands), Command(Command), plug )
import Tablebot.Plugin.Discord (sendMessage)
import Tablebot.Plugin.SmartCommand ( MakeCommand(parseMK) )

-- | random example case takes an integer and two strings
-- outputs the integer multiplied by ten, and the strings swapped
swapOpts :: Integer -> String -> String -> Message -> DatabaseDiscord ()
swapOpts i a b m = void $ sendMessage m (pack $ show (i * 10) ++ " " ++ b ++ " " ++ a)

-- | implement the swap command
swap :: Command
swap =
  Command
    "swap"
    (parseMK swapOpts)

-- | create the swap plugin
swapPlugin :: Plugin
swapPlugin = plug {commands = [swap]}
