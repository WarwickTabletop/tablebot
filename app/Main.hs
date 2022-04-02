{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Default
import Data.Text (Text)
import Tablebot (BotConfig (..), runTablebotWithEnv)
import Tablebot.Plugins (allPlugins)

-- @main@ runs forever. This allows bot reloading by fully shutting down the bot and letting it restart.
main :: IO ()
main = runTablebotWithEnv allPlugins $ def {gamePlaying = Just "Cosmic Encounter", rootHelpText = Just rootBody}

rootBody :: Text
rootBody =
  "**Test Bot**\n\
  \This bot is for testing, so should not be trusted."
