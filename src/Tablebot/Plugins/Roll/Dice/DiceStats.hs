{-# LANGUAGE QualifiedDo #-}

-- |
-- Module      : Tablebot.Plugins.Roll.Dice.DiceStats
-- Description : Get statistics on particular expressions.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This plugin generates statistics based on the values of dice in given
-- expressions.
module Tablebot.Plugins.Roll.Dice.DiceStats (rangeExpr, rangeListValues, getStats) where

import Control.Monad.Exception
import Data.Bifunctor
import qualified Data.Distribution as D
import Data.Foldable
import Data.List
import qualified Data.Map as M
import Data.Monoid
import Tablebot.Plugins.Roll.Dice.DiceData
import Tablebot.Plugins.Roll.Dice.DiceEval
import Tablebot.Plugins.Roll.Dice.DiceFunctions
import Tablebot.Plugins.Roll.Dice.DiceStatsBase (Distribution)
import qualified Tablebot.Plugins.Roll.Dice.DistributionMonad as DM
import Tablebot.Plugins.Roll.Dice.SortedList as SL
import Tablebot.Utility.Exception (catchBot)

type DistributionSortedList = D.Distribution (SortedList Integer)

type DistributionList = D.Distribution [Integer]

-- | Get the most common values, the mean, and the standard deviation of a given
-- distribution.
getStats :: Distribution -> ([Integer], Double, Double)
getStats d = (modalOrder, D.expectation d, D.standardDeviation d)
  where
    vals = D.toList d
    modalOrder = fst <$> sortBy (\(_, r) (_, r') -> compare r' r) vals

rangeExpr :: (MonadException m) => Expr -> m Distribution
rangeExpr e = do
  ex <- range e
  return $ ex

rangeListValues :: (MonadException m) => ListValues -> m [Distribution]
rangeListValues lv = do
  lve <- rangeList lv
  let lvd = lve
      lvd' = D.toList lvd
  return $ D.fromList <$> zip' lvd'
  where
    head' [] = []
    head' (x : _) = [x]
    getHeads xs = (\(xs', p) -> (,p) <$> head' xs') =<< xs
    getTails xs = first (drop 1) <$> xs
    zip' xs = getHeads xs : zip' (getTails xs)

-- | Try and get the `Distribution` of the given value, throwing a
-- `MonadException` on failure.
range :: (MonadException m, Range a, ParseShow a) => a -> m Distribution
range a = propagateException (parseShow a) (range' a)

-- | Type class to get the overall range of a value.
--
-- A `Data.Distribution.Distribution` is a map of values to probabilities, and
-- has a variety of  functions that operate on them.
class (ParseShow a) => Range a where
  range' :: (MonadException m, ParseShow a) => a -> m Distribution

instance (Range a) => Range (MiscData a) where
  range' (MiscVar l) = range l
  range' (MiscIf i) = rangeIfExpr range i

instance (RangeList a) => RangeList (MiscData a) where
  rangeList' (MiscVar l) = rangeList l
  rangeList' (MiscIf i) = rangeIfExpr rangeList i

rangeIfExpr :: (MonadException m, Ord b) => (a -> m (D.Distribution b)) -> If a -> m (D.Distribution b)
rangeIfExpr func (If b t f) = do
  b' <- range b
  let mp = D.toMap b'
      canBeFalse = M.member 0 mp
      canBeTrue = not $ M.null $ M.filterWithKey (\k _ -> k /= 0) mp
      emptyExp = D.fromList @_ @Integer []
  t' <- if canBeTrue then func t else return emptyExp
  f' <- if canBeFalse then func f else return emptyExp
  return $
    DM.do
      b'' <- b'
      if b'' /= 0 then t' else f'

instance (Range a) => Range (Var a) where
  range' (Var _ a) = range a
  range' (VarLazy _ a) = range a

instance (RangeList a) => RangeList (Var a) where
  rangeList' (Var _ a) = rangeList a
  rangeList' (VarLazy _ a) = rangeList a

instance (ParseShow typ, Operation typ, Range sub) => Range (BinOp sub typ) where
  range' (BinOp a tas) = foldl' foldel (range a) tas
    where
      foldel at (typ, b) = do
        a' <- at
        b' <- range b
        return $ DM.do
          param1 <- a'
          param2 <- b'
          DM.return $ getOperation typ param1 param2

instance Range Expr where
  range' (Expr e) = range e
  range' (ExprMisc t) = range t

instance Range Term where
  range' (Term (BinOp a tas)) = foldl' foldel (range a) tas
    where
      foldel at (Div, b) = do
        a' <- at
        b' <- range b
        -- If 0 is always the denominator, the distribution will be empty.
        return $ DM.do
          param1 <- a'
          param2 <- (D.assuming (/= 0) b')
          DM.return $ getOperation Div param1 param2
      foldel at (typ, b) = do
        a' <- at
        b' <- range b
        return $ DM.do
          param1 <- a'
          param2 <- b'
          DM.return $ getOperation typ param1 param2

instance Range Negation where
  range' (Neg t) = (negate DM.<$>) <$> range t
  range' (NoNeg t) = range t

instance Range Expo where
  range' (NoExpo t) = range t
  range' (Expo t e) = do
    d <- range t
    d' <- range e
    -- if the exponent is always negative, the distribution will be empty
    return $ DM.do
      param1 <- d
      param2 <- D.assuming (>= 0) d'
      DM.return $ param1 ^ param2

instance Range Func where
  range' (NoFunc t) = range t
  range' (Func fi avs) = rangeFunction fi avs

instance Range NumBase where
  range' (Value i) = return $ DM.return i
  range' (NBParen (Paren e)) = range e

instance Range Base where
  range' (NBase nb) = range nb
  range' (DiceBase d) = range d
  range' b@(NumVar _) = evaluationException "cannot find range of variable" [parseShow b]

instance Range Die where
  range' (MkDie aDie) = case aDie of
    LazyDie d -> range (MkDie d)
    Die nb -> do
      nbr <- range nb
      return $
        DM.do
          nbV <- nbr
          D.uniform [1 .. nbV]
    CustomDie lv -> do
      dievs <- rangeList lv
      return $ dievs DM.>>= D.uniform

instance Range Dice where
  range' (Dice b d mdor) = do
    b' <- range b
    d' <- range d
    let e = DM.do
          diecount <- b'
          getDiceExperiment diecount d'
    adjustDice <- rangeDiceExperiment d' mdor
    return $! sum DM.<$> adjustDice e

-- | Get the distribution of values from a given number of (identically
-- distributed) values and the distribution of that value.
getDiceExperiment :: Integer -> Distribution -> DistributionSortedList
getDiceExperiment i d =
  DM.sequenceSL (replicate (fromInteger i) d)

-- | Go through each operator on dice and modify the `Experiment` representing
-- all possible collections of rolls, returning the `Experiment` produced on
-- finding `Nothing`.
rangeDiceExperiment :: (MonadException m) => Distribution -> [DieOpOption] -> m (DistributionSortedList -> DistributionSortedList)
rangeDiceExperiment die =
  fmap (appEndo . fold) . traverse (rangeDieOpExperiment die)

-- | Perform one dice operation on the given `Experiment`, possibly returning
-- a modified experiment representing the distribution of dice rolls.
rangeDieOpExperiment :: (MonadException m) => Distribution -> DieOpOption -> m (Endo DistributionSortedList)
rangeDieOpExperiment die (MkDieOpOption doo) = case doo of
  (DieOpOptionLazy o) -> rangeDieOpExperiment die (MkDieOpOption o)
  (DieOpOptionKD kd lhw) -> rangeDieOpExperimentKD kd lhw
  (Reroll rro cond lim) -> do
    limd <- range lim
    return $ Endo $ \is -> DM.do
      limit <- limd
      let newDie = mkNewDie limit
      rolls <- is
      let (count, cutdownRolls) = countTriggers limit rolls
      if count == 0
        then DM.return rolls
        else (cutdownRolls <>) DM.<$> getDiceExperiment count newDie
    where
      mkNewDie limitValue
        | rro = die
        | otherwise = D.assuming (\i -> not $ applyCompare cond i limitValue) die
      countTriggers limitValue = foldr (\i ~(c, xs') -> if applyCompare cond i limitValue then (c + 1, xs') else (c, i `SL.insert` xs')) (0, mempty)

-- | Perform a keep/drop operation on the `Experiment` of dice rolls.
rangeDieOpExperimentKD :: (MonadException m) => KeepDrop -> LowHighWhere -> m (Endo DistributionSortedList)
rangeDieOpExperimentKD kd (Where cond nb) = do
  nbDis <- range nb
  return $ Endo $ \is -> DM.do
    wherelimit <- nbDis
    SL.filter (\i -> keepDrop $ applyCompare cond i wherelimit) DM.<$> is
  where
    keepDrop
      | kd == Keep = id
      | otherwise = not
rangeDieOpExperimentKD kd (LH lw nb) = do
  nbd <- range nb
  return $ Endo $ \is -> DM.do
    kdlh <- nbd
    (keepF . lowHighF (fromInteger kdlh)) DM.<$> is
  where
    keepF = case kd of
      Keep -> fst
      Drop -> snd
    lowHighF = case lw of
      Low -> splitL
      High -> splitR

-- | Try and get the `DistributionList` of the given value, throwing a
-- `MonadException` on failure.
rangeList :: (MonadException m, RangeList a, ParseShow a) => a -> m DistributionList
rangeList a = propagateException (parseShow a) (rangeList' a)

-- | Type class to get the overall range of a list of values.
--
-- Only used within `DiceStats` as I have no interest in producing statistics on
-- lists
class (ParseShow a) => RangeList a where
  rangeList' :: (MonadException m, ParseShow a) => a -> m DistributionList

instance RangeList ListValuesBase where
  rangeList' (LVBList es) = do
    exprs <- mapM range es
    return $ DM.sequence exprs
  rangeList' (LVBParen (Paren lv)) = rangeList lv

instance RangeList ListValues where
  rangeList' (LVBase lvb) = rangeList lvb
  rangeList' (MultipleValues nb b) = do
    nbd <- range nb
    bd <- range b
    return $
      DM.do
        valNum <- nbd
        SL.toList DM.<$> getDiceExperiment valNum bd
  rangeList' (LVFunc fi avs) = rangeFunction fi avs
  rangeList' (ListValuesMisc m) = rangeList m
  rangeList' b@(LVVar _) = evaluationException "cannot find range of variable" [parseShow b]

rangeArgValue :: (MonadException m) => ArgValue -> m (D.Distribution ListInteger)
rangeArgValue (AVExpr e) = (LIInteger DM.<$>) <$> range e
rangeArgValue (AVListValues lv) = (LIList DM.<$>) <$> rangeList lv

rangeFunction :: (MonadException m, Ord j) => FuncInfoBase j -> [ArgValue] -> m (D.Distribution j)
rangeFunction fi exprs = do
  exprs' <- mapM rangeArgValue exprs
  let params = first (funcInfoFunc fi) <$> D.toList (DM.sequence exprs')
  D.fromList <$> foldAndIgnoreErrors params
  where
    foldAndIgnoreErrors = foldr (\(mv, p) mb -> catchBot ((: []) . (,p) <$> mv) (const (return [])) >>= \v -> mb >>= \b -> return (v ++ b)) (return [])
