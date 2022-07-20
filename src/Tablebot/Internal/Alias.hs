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
import Data.Text
import Database.Persist.Sqlite (BackendKey (SqlBackendKey))
import qualified Database.Persist.Sqlite as Sql
import Database.Persist.TH
import Discord.Types
import Tablebot.Internal.Types
import Tablebot.Utility.Database (selectList)
import Tablebot.Utility.Types (EnvDatabaseDiscord)

share
  [mkPersist sqlSettings, mkMigrate "aliasMigration"]
  [persistLowerCase|
Alias
    alias Text
    command Text
    type AliasType
    UniqueAlias alias type
    deriving Show
    deriving Eq
|]

getAliases :: UserId -> EnvDatabaseDiscord d (Maybe [Alias])
getAliases uid =
  (Just . fmap Sql.entityVal <$> selectList [AliasType Sql.<-. [AliasPublic, AliasPrivate uid]] [])
    `catch` (\(_ :: SomeException) -> return Nothing)
