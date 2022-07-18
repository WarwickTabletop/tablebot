{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Tablebot (BotConfig (..), runTablebotWithEnv)
import Tablebot.Plugins (allPlugins)

-- @main@ runs forever. This allows bot reloading by fully shutting down the bot and letting it restart.
main :: IO ()
main =
  runTablebotWithEnv allPlugins $
    BotConfig
      { gamePlaying = game,
        rootHelpText = rootBody
      }

game :: Text -> Text
game prefix = "with dice. Prefix is `" <> prefix <> "`. Call `" <> prefix <> "help` for help"

rootBody :: Text
rootBody =
  "**Tabletop Bot**\n\
  \This friendly little bot provides several tools to help with\
  \ the running of the Warwick Tabletop Games and Role-Playing Society Discord server."
