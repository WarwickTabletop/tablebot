-- |
-- Module      : Tablebot.Internal.Handler.Event
-- Description : The event handler for everything else.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module deals with other kinds of features - 'MessageChange',
-- 'ReactionAdd', 'ReactionDel' and 'Other'.
module Tablebot.Internal.Handler.Event
  ( parseMessageChange,
    parseReactionAdd,
    parseReactionDel,
    parseComponentRecv,
    parseApplicationCommandRecv,
    parseOther,
  )
where

import Control.Concurrent (readMVar)
import Control.Monad.RWS (MonadIO (liftIO), MonadReader (ask))
import qualified Data.Map as M
import Data.Text as T (drop, isPrefixOf, length)
import Discord.Interactions (Interaction (..), InteractionDataApplicationCommand (interactionDataApplicationCommandId), InteractionDataComponent (interactionDataComponentCustomId))
import Discord.Types (ChannelId, Event, MessageId, ReactionInfo)
import Tablebot.Internal.Plugins (changeAction)
import Tablebot.Internal.Types as IT
import qualified Tablebot.Utility.Types as UT

-- | This runs each 'MessageChange' feature in @cs@ with the information from a
-- Discord 'MessageUpdate' or 'MessageDelete' event - whether it is an update
-- or delete (the bool @updated@), the 'ChannelId' @cid@, and the 'MessageId'
-- @mid@.
parseMessageChange ::
  [CompiledMessageChange] ->
  Bool ->
  ChannelId ->
  MessageId ->
  CompiledDatabaseDiscord ()
parseMessageChange cs updated cid mid = mapM_ doMessageChange cs
  where
    doMessageChange c = onMessageChange c updated cid mid

-- | This runs each 'ReactionAdd' feature in @cs@ with the information from a
-- Discord 'MessageReactionAdd' event provided as 'ReactionInfo' @info@.
parseReactionAdd :: [CompiledReactionAdd] -> ReactionInfo -> CompiledDatabaseDiscord ()
parseReactionAdd cs info = mapM_ doReactionAdd cs
  where
    doReactionAdd c = onReactionAdd c info

-- | This runs each 'ReactionDel' feature in @cs@ with the information from
-- a Discord 'MessageReactionRemove' event provided as 'ReactionInfo' @info@.
parseReactionDel :: [CompiledReactionDel] -> ReactionInfo -> CompiledDatabaseDiscord ()
parseReactionDel cs info = mapM_ doReactionAdd cs
  where
    doReactionAdd c = onReactionDelete c info

parseComponentRecv :: [CompiledComponentRecv] -> Interaction -> CompiledDatabaseDiscord ()
parseComponentRecv cs info@InteractionComponent {interactionDataComponent = Just idc} = mapM_ removePrefix cs'
  where
    getPrefix ccr = componentPluginName ccr <> componentName ccr
    cs' = filter (\ccr -> getPrefix ccr `isPrefixOf` interactionDataComponentCustomId idc) cs
    removePrefix ccr = ccr `onComponentRecv` (info {interactionDataComponent = Just (idc {interactionDataComponentCustomId = T.drop (T.length (getPrefix ccr)) (interactionDataComponentCustomId idc)})})
parseComponentRecv _ _ = return ()

parseApplicationCommandRecv :: Interaction -> CompiledDatabaseDiscord ()
parseApplicationCommandRecv info@InteractionApplicationCommand {interactionDataApplicationCommand = Just idac} = do
  tvar <- ask
  cache <- liftIO $ readMVar tvar
  let action = UT.cacheApplicationCommands cache M.!? interactionDataApplicationCommandId idac
  case action of
    Nothing -> return ()
    Just act -> changeAction () $ act info
parseApplicationCommandRecv _ = return ()

-- | This runs each 'Other' feature in @cs@ with the Discord 'Event' provided.
-- Note that any events covered by other feature types will /not/ be run
-- through this.
parseOther :: [CompiledOther] -> Event -> CompiledDatabaseDiscord ()
parseOther cs ev = mapM_ doOther cs
  where
    doOther c = onOtherEvent c ev
