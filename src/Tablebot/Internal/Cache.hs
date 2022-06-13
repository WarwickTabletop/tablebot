-- |
-- Module      : Tablebot.Internal.Cache
-- Description : A content cache for internal bot use.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A cache for data needed by the bot to provide features without making multiple calls to discord apis.
-- Not intended for use by plugins directly, if you need to do that create a separate cache in your setup phase.
module Tablebot.Internal.Cache where

import Control.Monad.Trans.Reader (ask)
import qualified Data.Map as M
import Data.Text (Text)
import Discord.Types
import Tablebot.Utility.Types
import UnliftIO (atomically, readTVarIO, writeTVar)

lookupEmojiCache :: Text -> EnvDatabaseDiscord s (Maybe Emoji)
lookupEmojiCache t = do
  mcache <- liftCache ask
  cache <- readTVarIO mcache
  pure $ M.lookup t $ cacheKnownEmoji cache

insertEmojiCache :: Text -> Emoji -> EnvDatabaseDiscord s ()
insertEmojiCache t e = do
  mcache <- liftCache ask
  cache <- readTVarIO mcache
  let new = cache {cacheKnownEmoji = M.insert t e (cacheKnownEmoji cache)}
  atomically $ writeTVar mcache new

addNewEmojiCache :: Text -> Emoji -> EnvDatabaseDiscord s ()
addNewEmojiCache t e = do
  mcache <- liftCache ask
  cache <- readTVarIO mcache
  let emap = cacheKnownEmoji cache
      new = cache {cacheKnownEmoji = if M.member t emap then emap else M.insert t e emap}
  atomically $ writeTVar mcache new

fillEmojiCache :: Guild -> EnvDatabaseDiscord s ()
fillEmojiCache guild = do
  let emoji = guildEmojis guild
  mapM_ (addNewEmojiCache =<< emojiName) emoji

getVersionInfo :: EnvDatabaseDiscord s VersionInfo
getVersionInfo = do
  mcache <- liftCache ask
  cache <- readTVarIO mcache
  pure $ cacheVersionInfo cache
