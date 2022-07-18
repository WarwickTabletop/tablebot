-- |
-- Module      : Tablebot.Utility.Permission
-- Description : A simple interface to allow plugins to handle permissions themselves
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This contains a simple interface for plugin authors to require a specific level of privilege.
module Tablebot.Utility.Permission where

import Tablebot.Internal.Permission
import Tablebot.Utility.Exception (BotException (PermissionException), throwBot)
import Tablebot.Utility.Types

-- | @requirePermission@ only runs the inputted effect if permissions are matched. Otherwise it returns an error.
requirePermission :: Context m => RequiredPermission -> m -> EnvDatabaseDiscord s a -> EnvDatabaseDiscord s a
requirePermission perm m a = do
  p <- getSenderPermission m
  if userHasPermission perm p
    then a
    else throwBot $ PermissionException "Sorry, you don't have permission to do that."
