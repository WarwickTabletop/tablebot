{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MockDiscord where

import Control.Concurrent (MVar, ThreadId, forkIO, killThread, newEmptyMVar)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (IOException, try)
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import Discord
import Discord.Handle (DiscordHandle (..), HandleThreadId (..))
import Discord.Internal.Gateway (Event, GatewayException, GatewayHandle (GatewayHandle), startCacheThread)
import Discord.Internal.Rest
import Discord.Internal.Rest.HTTP (JsonRequest, RestCallInternalException)
import System.Random (randomIO)

data MockDiscord = MockDiscord
  { -- | A computation which shuts down all threads associated with MockDiscord.
    shutdownAction :: IO (),
    -- | The bot makes requests via this channel. You have to read this manually.
    sendEvents :: Chan (String, JsonRequest, MVar (Either RestCallInternalException BL.ByteString)),
    -- | The bot receives events via this channel, and runs its handler on each. You can send messages via this.
    receiveEvents :: Chan (Either GatewayException Event)
  }

-- | In order to run DatabaseDiscord () while testing, we need a fake Discord connection.
-- This creates such a fake Discord connection with the input event handler.
-- Returns a MockDiscord, which can be used to interact with discord-haskell.
makeMockDiscord :: (Event -> DiscordHandler ()) -> IO MockDiscord
makeMockDiscord handler = do
  log <- newChan
  logId <- startLogger print log
  (cache, cacheId) <- startCacheThread log
  -- TODO: as Anna points out, we will need to do some stuff with the REST channel.
  -- Have a second channel that's tester-facing, and a thread that takes REST requests
  -- in order to act on them, either simplifying requests to more useful data (e.g.
  -- identifying messages), or performing necessary operations (like summoning a fake
  -- emoji).
  restChan <- newChan
  gatewayChan <- newChan
  libE <- newEmptyMVar

  let handle =
        DiscordHandle
          { discordHandleRestChan = RestChanHandle restChan,
            -- Left as undefined due to limited scope of this mock.
            -- TODO: deal with user sendables, updating the various bits.
            discordHandleGateway = GatewayHandle gatewayChan undefined undefined undefined undefined,
            discordHandleCache = cache,
            discordHandleLog = log,
            discordHandleLibraryError = libE,
            discordHandleThreads = []
          }

  gatewayId <-
    forkIO $
      forever $
        readChan gatewayChan >>= \case
          Left err -> print err
          Right ev -> runReaderT (handler ev) handle

  return $
    MockDiscord
      { shutdownAction = mapM_ killThread [cacheId, gatewayId],
        sendEvents = restChan,
        receiveEvents = gatewayChan
      }

defUser :: User
defUser =
  User
    { userId = Snowflake 0,
      userName = "Test User",
      userDiscrim = "0000",
      userAvatar = Nothing,
      userIsBot = False,
      userIsWebhook = False,
      userMfa = Nothing,
      userVerified = Nothing,
      userEmail = Nothing
    }

userSendsMessage :: MockDiscord -> T.Text -> IO MessageId
userSendsMessage (MockDiscord _ _ re) message = do
  mid <- Snowflake <$> randomIO
  timeNow <- systemToUTCTime <$> getSystemTime
  let mess =
        Message
          { messageId = mid,
            messageChannel = 0, -- assuming only one channel in these tests
            messageAuthor = defUser,
            messageText = message,
            messageTimestamp = timeNow,
            messageEdited = Nothing,
            messageTts = False,
            messageEveryone = False,
            messageMentions = [],
            messageMentionRoles = [],
            messageAttachments = [],
            messageEmbeds = [],
            messageReactions = [],
            messageNonce = Nothing,
            messagePinned = False,
            messageGuild = Nothing,
            messageReference = Nothing,
            referencedMessage = Nothing
          }
  writeChan re (Right $ MessageCreate mess)
  return mid

-- Taken direct from discord-haskell, since it is not exported.
startLogger :: (T.Text -> IO ()) -> Chan T.Text -> IO ThreadId
startLogger handle logC = forkIO $
  forever $
    do
      me <- try $ readChan logC >>= handle
      case me of
        Right _ -> pure ()
        Left (_ :: IOException) ->
          -- writeChan logC "Log handler failed"
          pure ()
