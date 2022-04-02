{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Tablebot
-- Description : The main runner for the Tablebot Discord bot.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the main runner for Tablebot. If you're just looking to
-- run the bot with existing plugins, import this and your favourite plugins
-- from "Tablebot.Plugins".
module Tablebot
  ( runTablebot,
    runTablebotWithEnv,
    BotConfig (..),
  )
where

import Control.Concurrent
import Control.Monad.Extra
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (NoLoggingT (runNoLoggingT))
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO (putStrLn)
import Database.Persist.Sqlite
  ( createSqlitePool,
    runMigration,
    runSqlPool,
  )
import Discord
import Discord.Internal.Rest
import LoadEnv (loadEnv)
import Paths_tablebot (version)
import System.Environment (getEnv, lookupEnv)
import System.Exit (die)
import Tablebot.Handler (eventHandler, killCron, runCron)
import Tablebot.Internal.Administration
import Tablebot.Internal.Plugins
import Tablebot.Internal.Types
import Tablebot.Plugins (addAdministrationPlugin)
import Tablebot.Utility
import Tablebot.Utility.Help
import Text.Regex.PCRE ((=~))

-- TODO: Make a Config type or something which can be passed in to set various bits.
-- All plugins that rely on it can have that as an Env argument, found via MVar or similar.

-- | runTablebotWithEnv @plugins@ runs the bot using data found in the .env
-- file with the @[CompiledPlugin]@ given. If you're looking to run the bot as
-- detailed in the README (i.e. using data from .env), you should call this
-- function.
runTablebotWithEnv :: [CompiledPlugin] -> BotConfig -> IO ()
runTablebotWithEnv plugins config = do
  -- fetch the version info as soon after building to reduce the likelihood that it changes between build and run
  gv <- gitVersion
  let vInfo = VInfo gv version
  rFlag <- newMVar Reload :: IO (MVar ShutdownReason)
  whileM $ do
    _ <- swapMVar rFlag Reload
    loadEnv
    dToken <- pack <$> getEnv "DISCORD_TOKEN"
    unless (encodeUtf8 dToken =~ ("^[A-Za-z0-9_-]{24}[.][A-Za-z0-9_-]{6}[.][A-Za-z0-9_-]{27}$" :: String)) $
      die "Invalid token format. Please check it is a bot token"
    prefix <- pack . fromMaybe "!" <$> lookupEnv "PREFIX"
    dbpath <- getEnv "SQLITE_FILENAME"
    runTablebot vInfo dToken prefix dbpath (addAdministrationPlugin rFlag plugins) config
    exit <- swapMVar rFlag Reload
    restartAction exit
    pure $ not (restartIsTerminal exit)
  putStrLn "Bot closed"

-- | runTablebot @dToken@ @prefix@ @dbpath@ @plugins@ @config@ runs the bot
-- using the given Discord API token @dToken@ and SQLite connection string
-- @dbpath@. Only the plugins provided by @plugins@ are run, and all commands
-- are prefixed with @prefix@. @config@ details how the bot should present
-- itself to users, allowing programmers to replace the Tablebot-specific text
-- with their own.
-- The plugins given are combined into a single plugin with their combined
-- functionality. Each migration present in the combined plugin is run, and
-- each cron job and handler is set up.
-- This creates a small pool of database connections used by the event handler,
-- builds an event handler and starts cron jobs. It also kills the cron jobs on
-- bot close.
runTablebot :: VersionInfo -> Text -> Text -> FilePath -> [CompiledPlugin] -> BotConfig -> IO ()
runTablebot vinfo dToken prefix dbpath plugins config =
  do
    debugPrint ("DEBUG enabled. This is strongly not recommended in production!" :: String)
    -- Create multiple database threads.
    pool <- runNoLoggingT $ createSqlitePool (pack dbpath) 8

    -- Setup and then apply plugin blacklist from the database
    runSqlPool (runMigration adminMigration) pool
    blacklist <- runResourceT $ runNoLoggingT $ runSqlPool currentBlacklist pool
    let filteredPlugins = removeBlacklisted blacklist plugins
    -- Combine the list of plugins into both a combined plugin
    let !plugin = generateHelp (rootHelpText config) $ combinePlugins filteredPlugins
    -- Run the setup actions of each plugin and collect the plugin actions into a single @PluginActions@ instance
    allActions <- mapM (runResourceT . runNoLoggingT . flip runSqlPool pool) (combinedSetupAction plugin)
    let !actions = combineActions allActions

    -- TODO: this might have issues with duplicates?
    -- TODO: in production, this should probably run once and then never again.
    mapM_ (\migration -> runSqlPool (runMigration migration) pool) $ combinedMigrations plugin
    -- Create a var to kill any ongoing tasks.
    mvar <- newEmptyMVar :: IO (MVar [ThreadId])
    cacheMVar <- newMVar (TCache M.empty vinfo) :: IO (MVar TablebotCache)
    userFacingError <-
      runDiscord $
        def
          { discordToken = dToken,
            discordOnEvent =
              flip runSqlPool pool . flip runReaderT cacheMVar . eventHandler actions prefix,
            discordOnStart = do
              -- Build list of cron jobs, saving them to the mvar.
              -- Note that we cannot just use @runSqlPool@ here - this creates
              -- a single transaction which is reverted in case of exception
              -- (which can just happen due to databases being unavailable
              -- sometimes).
              runReaderT (mapM (runCron pool) (compiledCronJobs actions) >>= liftIO . putMVar mvar) cacheMVar
              liftIO $ putStrLn "The bot lives!"
              sendCommand (UpdateStatus activityStatus),
            -- Kill every cron job in the mvar.
            discordOnEnd = takeMVar mvar >>= killCron
          }
    TIO.putStrLn userFacingError
  where
    activityStatus =
      UpdateStatusOpts
        { updateStatusOptsSince = Nothing,
          updateStatusOptsGame =
            Just
              ( Activity
                  { activityName = gamePlaying config <> ". Prefix is `" <> prefix <> "`. Call `" <> prefix <> "help` for help",
                    activityType = ActivityTypeGame,
                    activityUrl = Nothing
                  }
              ),
          updateStatusOptsNewStatus = UpdateStatusOnline,
          updateStatusOptsAFK = False
        }
