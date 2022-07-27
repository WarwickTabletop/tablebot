{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : Tablebot.Plugins.Alias
-- Description : Alias plugin
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Allows users to add, list, and delete aliases.
module Tablebot.Plugins.Alias (alias, Alias (..), getAliases) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Database.Persist.Sqlite as Sql
import Discord.Types
import Tablebot.Internal.Alias
import Tablebot.Internal.Types (AliasType (..))
import Tablebot.Utility
import Tablebot.Utility.Database (deleteBy, exists)
import Tablebot.Utility.Discord (sendMessage)
import Tablebot.Utility.Permission (requirePermission)
import Tablebot.Utility.SmartParser (PComm (parseComm), Quoted (..), WithError (..))
import Text.RawString.QQ (r)

publicAliasPerms :: RequiredPermission
publicAliasPerms = Moderator

alias :: CompiledPlugin
alias = compilePlugin aliasPlugin

aliasTypeToText :: AliasType -> Text
aliasTypeToText AliasPublic = "Public"
aliasTypeToText (AliasPrivate _) = "Private"

updateAlias :: Alias -> EnvDatabaseDiscord d (Sql.Entity Alias)
updateAlias a = liftSql $ Sql.upsertBy (UniqueAlias (aliasAlias a) (aliasType a)) a [AliasCommand Sql.=. aliasCommand a]

aliasPlugin :: Plugin
aliasPlugin =
  (plug "alias")
    { commands = [aliasComm],
      migrations = [aliasMigration],
      helpPages = [aliasHelp]
    }

aliasComm :: Command
aliasComm =
  Command
    "alias"
    (parseComm (\m -> aliasList (AliasPrivate (userId $ messageAuthor m)) m))
    [aliasAddCommand, aliasListCommand, aliasDeleteCommand, commandAlias "remove" aliasDeleteCommand]

aliasHelp :: HelpPage
aliasHelp =
  HelpPage
    "alias"
    []
    "alias a command to another command"
    [r|**Aliases**
Allows creation, viewing, and deletion of aliases.
Calling without any arguments will show a list of aliases. 

*Usage:* `alias`|]
    [aliasAddHelp, aliasListHelp, aliasDeleteHelp]
    None

aliasAddCommand :: Command
aliasAddCommand =
  Command
    "add"
    (parseComm aliasAddPrivateComm)
    [ Command "private" (parseComm aliasAddPrivateComm) [],
      Command "public" (parseComm aliasAddPublicComm) []
    ]
  where
    aliasAddPrivateComm :: WithError "Need a single word" Text -> WithError "Need a quoted string" (Quoted Text) -> Message -> DatabaseDiscord ()
    aliasAddPrivateComm (WErr t) (WErr (Qu t')) m = aliasAdd t t' (AliasPrivate (userId $ messageAuthor m)) m
    aliasAddPublicComm :: WithError "Need a single word" Text -> WithError "Need a quoted string" (Quoted Text) -> Message -> DatabaseDiscord ()
    aliasAddPublicComm (WErr t) (WErr (Qu t')) m = requirePermission publicAliasPerms m $ aliasAdd t t' AliasPublic m

aliasAdd :: Text -> Text -> AliasType -> Message -> DatabaseDiscord ()
aliasAdd a b at m = do
  let new = Alias a b at
  _ <- updateAlias new
  sendMessage m ("Added " <> T.toLower (aliasTypeToText at) <> " alias `" <> a <> "` -> `" <> b <> "`")

aliasAddHelp :: HelpPage
aliasAddHelp =
  HelpPage
    "add"
    []
    "adds an alias"
    [r|**Add Alias**
Adds an alias.

*Usage:* `alias add <alias> "<command>"`|]
    [ HelpPage "private" [] "adds a private alias" "**Add Private Alias**\nAdds a private alias.\n\n*Usage:* `alias add private <alias> \"<command>\"`" [] None,
      HelpPage "public" [] "adds a public alias" "**Add Public Alias**\nAdds a public alias.\n\n*Usage:* `alias add public <alias> \"<command>\"`" [] publicAliasPerms
    ]
    None

aliasListCommand :: Command
aliasListCommand =
  Command
    "list"
    (parseComm aliasListPrivateComm)
    [ Command "private" (parseComm aliasListPrivateComm) [],
      Command "public" (parseComm aliasListPublicComm) []
    ]
  where
    aliasListPrivateComm :: Message -> DatabaseDiscord ()
    aliasListPrivateComm m = aliasList (AliasPrivate (userId $ messageAuthor m)) m
    aliasListPublicComm :: Message -> DatabaseDiscord ()
    aliasListPublicComm m = aliasList AliasPublic m

aliasList :: AliasType -> Message -> DatabaseDiscord ()
aliasList at m = do
  aliases <- fmap Sql.entityVal <$> liftSql (Sql.selectList [AliasType Sql.==. at] [])
  let msg =
        aliasTypeToText at <> " aliases:\n"
          <> T.unlines (map (\(Alias a b _) -> "\t`" <> a <> "` -> `" <> b <> "`") aliases)
  sendMessage m msg

aliasListHelp :: HelpPage
aliasListHelp =
  HelpPage
    "list"
    []
    "lists aliases"
    [r|**List Aliases**
Lists all aliases.
You can specify whether the aliases are public or private.

*Usage:* `alias list`|]
    [ HelpPage "private" [] "lists your private aliases" "**List Private Aliases**\nLists your private aliases.\n\n*Usage:* `alias list private`" [] None,
      HelpPage "public" [] "lists the public aliases" "**List Public Aliases**\nLists the public aliases.\n\n*Usage:* `alias list public`" [] None
    ]
    None

aliasDeleteCommand :: Command
aliasDeleteCommand =
  Command
    "delete"
    (parseComm aliasDeletePrivateComm)
    [ Command "private" (parseComm aliasDeletePrivateComm) [],
      Command "public" (parseComm aliasDeletePublicComm) []
    ]
  where
    aliasDeletePrivateComm :: WithError "Need a single word" Text -> Message -> DatabaseDiscord ()
    aliasDeletePrivateComm (WErr t) m = aliasDelete t (AliasPrivate (userId $ messageAuthor m)) m
    aliasDeletePublicComm :: WithError "Need a single word" Text -> Message -> DatabaseDiscord ()
    aliasDeletePublicComm (WErr t) m = requirePermission publicAliasPerms m $ aliasDelete t AliasPublic m

aliasDelete :: Text -> AliasType -> Message -> DatabaseDiscord ()
aliasDelete a at m = do
  let toDelete = UniqueAlias a at
  itemExists <- exists [AliasAlias Sql.==. a, AliasType Sql.==. at]
  if itemExists
    then deleteBy toDelete >> sendMessage m ("Deleted alias `" <> a <> "`")
    else sendMessage m ("No such alias `" <> a <> "`")

aliasDeleteHelp :: HelpPage
aliasDeleteHelp =
  HelpPage
    "delete"
    ["remove"]
    "deletes an alias"
    [r|**Delete Alias**
Deletes a private alias.

*Usage:* `alias delete <alias>`|]
    [ HelpPage "private" [] "deletes a private alias" "**Delete Private Alias**\nDeletes a private alias.\n\n*Usage:* `alias delete private <alias>`" [] None,
      HelpPage "public" [] "deletes a public alias" "**Delete Public Alias**\nDeletes a public alias.\n\n*Usage:* `alias delete public <alias>`" [] publicAliasPerms
    ]
    None
