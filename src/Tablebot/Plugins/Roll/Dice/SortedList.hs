module Tablebot.Plugins.Roll.Dice.SortedList
  ( SortedList,
    toList,
    fromList,
    insert,
    Tablebot.Plugins.Roll.Dice.SortedList.filter,
    splitL,
    splitR,
  )
where

import Data.Bifunctor
import qualified Data.Foldable
import qualified Data.Map.Strict as M

newtype SortedList a = MkSortedList (M.Map a Int)
  deriving (Eq, Ord, Show)

fromList :: (Ord a) => [a] -> SortedList a
fromList = MkSortedList . M.fromListWith (+) . fmap (,1)

toList :: SortedList a -> [a]
toList (MkSortedList m) = do
  (a, incidence) <- M.toAscList m
  replicate incidence a

insert :: (Ord a) => a -> SortedList a -> SortedList a
insert a (MkSortedList m) = MkSortedList $ M.insertWith (+) a 1 m

filter :: (a -> Bool) -> SortedList a -> SortedList a
filter p (MkSortedList m) = MkSortedList $ M.filterWithKey (\k _ -> p k) m

splitR :: (Ord a) => Int -> SortedList a -> (SortedList a, SortedList a)
splitR i sl@(MkSortedList m)
  | i <= 0 = (mempty, sl)
  | otherwise =
      bimap (MkSortedList . M.fromDescList) (MkSortedList . M.fromDescList) $
        splitCount i (M.toDescList m)

splitL :: (Ord a) => Int -> SortedList a -> (SortedList a, SortedList a)
splitL i sl@(MkSortedList m)
  | i <= 0 = (mempty, sl)
  | otherwise =
      bimap (MkSortedList . M.fromAscList) (MkSortedList . M.fromAscList) $
        splitCount i (M.toAscList m)

-- | Split the list by using the int values associated with each value as a count.
splitCount :: Int -> [(a, Int)] -> ([(a, Int)], [(a, Int)])
splitCount 0 as = ([], as)
splitCount _ [] = ([], [])
splitCount i (aTup@(a, i') : as)
  | i >= i' = first (aTup :) $ splitCount (i - i') as
  | otherwise = ([(a, i)], (a, i' - i) : as)

instance Foldable SortedList where
  foldMap f = foldMap f . toList
  sum (MkSortedList m) =
    M.foldlWithKey'
      (\acc k incidence -> acc + k * (fromIntegral incidence))
      0
      m
  toList = Tablebot.Plugins.Roll.Dice.SortedList.toList
  length (MkSortedList m) = sum m

instance (Ord a) => Semigroup (SortedList a) where
  (MkSortedList m1) <> (MkSortedList m2) = MkSortedList (M.unionWith (+) m1 m2)

instance (Ord a) => Monoid (SortedList a) where
  mempty = MkSortedList M.empty
