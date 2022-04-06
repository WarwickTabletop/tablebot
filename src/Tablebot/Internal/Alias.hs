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

import Database.Persist.Sqlite
import Discord.Types (Snowflake (Snowflake), UserId)

data AliasType = AliasPublic | AliasPrivate UserId
  deriving (Eq, Show, Ord)

instance PersistField AliasType where
  toPersistValue (AliasPrivate (Snowflake wd)) = PersistInt64 (fromIntegral wd)
  toPersistValue AliasPublic = PersistInt64 (-1)
  fromPersistValue = \case
    PersistInt64 (-1) -> Right AliasPublic
    PersistInt64 i -> Right $ AliasPrivate (fromIntegral i)
    _ -> Left "AliasType: fromPersistValue: Invalid value"

instance PersistFieldSql AliasType where
  sqlType _ = SqlInt64
