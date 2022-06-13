-- -- |
-- Module      : Tablebot.Utility.Help
-- Description : Help text generation and storage
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module creates functions and data structures to help generate help text for commands
module Tablebot.Utility.Help where

import Control.Monad.Reader (MonadReader (ask))
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import Discord.Types
import Tablebot.Internal.Permission (getSenderPermission, userHasPermission)
import Tablebot.Internal.Plugins (changeAction)
import Tablebot.Internal.Types
import Tablebot.Utility.Discord
import Tablebot.Utility.Embed (addColour)
import Tablebot.Utility.Parser (skipSpace)
import Tablebot.Utility.Permission (requirePermission)
import Tablebot.Utility.Types hiding (helpPages)
import Text.Megaparsec (choice, chunk, eof, try, (<?>), (<|>))
import UnliftIO (readTVarIO)

helpHelpPage :: HelpPage
helpHelpPage = HelpPage "help" [] "show information about commands" "**Help**\nShows information about bot commands\n\n*Usage:* `help <page>`" [] None

generateHelp :: Text -> CombinedPlugin -> CombinedPlugin
generateHelp rootText p =
  p
    { combinedSetupAction = return (PA [CCommand "help" (handleHelp rootText (helpHelpPage : combinedHelpPages p)) []] [] [] [] [] [] []) : combinedSetupAction p
    }

handleHelp :: Text -> [HelpPage] -> Parser (Message -> CompiledDatabaseDiscord ())
handleHelp rootText hp = parseHelpPage True root
  where
    root = HelpPage "" [] "" rootText hp None

parseHelpPage :: Bool -> HelpPage -> Parser (Message -> CompiledDatabaseDiscord ())
parseHelpPage isRoot hp = do
  _ <- choice (map chunk (helpName hp : helpAliases hp))
  skipSpace
  (try eof $> displayHelp isRoot hp) <|> choice (map (parseHelpPage False) $ helpSubpages hp) <?> "Unknown Subcommand"

displayHelp :: Bool -> HelpPage -> Message -> CompiledDatabaseDiscord ()
displayHelp isRoot hp m = changeAction () . requirePermission (helpPermission hp) m $ do
  uPerm <- getSenderPermission m
  cache <- liftCache ask
  botName' <- botName . cacheBotInfo <$> readTVarIO cache
  sendEmbedMessage m "" $ addColour Aqua $ createEmbed $ CreateEmbed "" "" Nothing (formatHelpTitle hp botName') "" Nothing (formatHelp isRoot uPerm hp) [] Nothing "" Nothing Nothing

formatHelpTitle :: HelpPage -> Text -> Text
formatHelpTitle hp botName' = ":scroll:  " <> if helpName hp == "" then botName' else "Help: `$" <> helpName hp <> "`"

formatHelp :: Bool -> UserPermission -> HelpPage -> Text
formatHelp isRoot up hp = helpBody hp <> formatSubpages hp
  where
    formatSubpages :: HelpPage -> Text
    formatSubpages (HelpPage _ _ _ _ [] _) = ""
    formatSubpages hp' =
      if T.null sp
        then ""
        else (if isRoot then "\n\n*Commands*" else "\n\n*Subcommands*") <> sp
      where
        sp = T.concat (map formatSubpage (helpSubpages hp'))
    formatSubpage :: HelpPage -> Text
    formatSubpage hp' = if userHasPermission (helpPermission hp') up then "\n`" <> helpName hp' <> "` " <> helpShortText hp' else ""
