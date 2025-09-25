{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QualifiedDo #-}

module Tablebot.Plugins.Roll.Dice.DistributionMonad where

import Data.Distribution.Core
import Data.Ord (Ord)
import Data.Function (id)
import Data.Monoid
import Tablebot.Plugins.Roll.Dice.SortedList as SL

(>>=) :: (Ord b) => Distribution a -> (a -> Distribution b) -> Distribution b
(>>=) = andThen

return :: a -> Distribution a
return = always

(<$>) :: Ord b => (a -> b) -> Distribution a -> Distribution b
(<$>) = select

traverse :: Ord b => (a -> Distribution b) -> [a] -> Distribution [b]
traverse _ [] = return []
traverse f (a : as) =
  f a >>= \b -> (b :) <$> traverse f as

sequence :: (Ord a) => [Distribution a] -> Distribution [a]
sequence = traverse id

sequenceSL :: (Ord a) => [Distribution a] -> Distribution (SortedList a)
sequenceSL [] = return mempty
sequenceSL (da : das) = da >>= \a -> SL.insert a <$> sequenceSL das
