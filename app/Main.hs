{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Tablebot (BotConfig (..), runTablebotWithEnv)
import Tablebot.Plugins (allPlugins, minusPl)

-- @main@ runs forever. This allows bot reloading by fully shutting down the bot and letting it restart.
main :: IO ()
main = runTablebotWithEnv (allPlugins `minusPl` ["ping"]) $ BotConfig {gamePlaying = "with dice", rootHelpText = rootBody}

rootBody :: Text
rootBody =
  "**Tabletop Bot**\n\
  \This friendly little bot provides several tools to help with\
  \ the running of the Warwick Tabletop Games and Role-Playing Society Discord server."
