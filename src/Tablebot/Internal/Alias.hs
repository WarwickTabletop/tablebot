-- |
-- Module      : Tablebot.Internal.Alias
-- Description : Alias management
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Alias management
module Tablebot.Internal.Alias where

import Control.Monad.Exception (MonadException (catch), SomeException)
import qualified Data.Text as T
import Database.Persist.Sqlite (BackendKey (SqlBackendKey))
import qualified Database.Persist.Sqlite as Sql
import Database.Persist.TH
import Discord.Types
import Tablebot.Internal.Administration (currentBlacklist)
import Tablebot.Internal.Types
import Tablebot.Utility.Database (liftSql, selectList)
import Tablebot.Utility.Types (EnvDatabaseDiscord)

share
  [mkPersist sqlSettings, mkMigrate "aliasMigration"]
  [persistLowerCase|
Alias
    alias T.Text
    command T.Text
    type AliasType
    UniqueAlias alias type
    deriving Show
    deriving Eq
|]

getAliases :: UserId -> EnvDatabaseDiscord d (Maybe [Alias])
getAliases uid = do
  blacklist <- liftSql currentBlacklist
  if "alias" `elem` blacklist
    then return Nothing
    else
      (Just . fmap Sql.entityVal <$> selectList [AliasType Sql.<-. [AliasPublic, AliasPrivate uid]] [])
        `catch` (\(_ :: SomeException) -> return Nothing)
