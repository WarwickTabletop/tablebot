module Main where

import Tablebot (runTablebotWithEnv)
import Tablebot.Plugins (allPlugins)

-- @main@ runs forever. This allows bot reloading by fully shutting down the bot and letting it restart.
main :: IO ()
main = runTablebotWithEnv allPlugins
