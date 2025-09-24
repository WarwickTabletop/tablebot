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
import Data.Bifunctor (Bifunctor (first))
import qualified Data.Distribution as D
import Data.List
import qualified Data.Map as M
import Tablebot.Plugins.Roll.Dice.DiceData
import Tablebot.Plugins.Roll.Dice.DiceEval
import Tablebot.Plugins.Roll.Dice.DiceFunctions
import Tablebot.Plugins.Roll.Dice.DiceStatsBase (Distribution)
import Tablebot.Utility.Exception (catchBot)

import qualified Tablebot.Plugins.Roll.Dice.DistributionMonad as DM

type Experiment = D.Distribution Integer

type ExperimentList = D.Distribution [Integer]

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

-- | Type class to get the overall range of a value.
--
-- A `Data.Distribution.Distribution` is a map of values to probabilities, and
-- has a variety of  functions that operate on them.
--
-- An `Data.Distribution.Experiment` is a monadic form of this.
class (ParseShow a) => Range a where
  -- | Try and get the `Experiment` of the given value, throwing a
  -- `MonadException` on failure.
  range :: (MonadException m, ParseShow a) => a -> m Experiment
  range a = propagateException (parseShow a) (range' a)

  range' :: (MonadException m, ParseShow a) => a -> m Experiment

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
    res <- rangeDiceExperiment d' mdor e
    return $ sum DM.<$> res

-- | Get the distribution of values from a given number of (identically
-- distributed) values and the distribution of that value.
getDiceExperiment :: Integer -> Distribution -> ExperimentList
getDiceExperiment i d = DM.sequence $ replicate (fromInteger i) d

-- | Go through each operator on dice and modify the `Experiment` representing
-- all possible collections of rolls, returning the `Experiment` produced on
-- finding `Nothing`.
rangeDiceExperiment :: (MonadException m) => Experiment -> [DieOpOption] -> ExperimentList -> m ExperimentList
rangeDiceExperiment _ [] is = return is
rangeDiceExperiment die (doo : doos) is = rangeDieOpExperiment die doo is >>= rangeDiceExperiment die doos

-- | Perform one dice operation on the given `Experiment`, possibly returning
-- a modified experiment representing the distribution of dice rolls.
rangeDieOpExperiment :: (MonadException m) => Experiment -> DieOpOption -> ExperimentList -> m ExperimentList
rangeDieOpExperiment die (MkDieOpOption doo) is = case doo of
  (DieOpOptionLazy o) -> rangeDieOpExperiment die (MkDieOpOption o) is
  (DieOpOptionKD kd lhw) -> rangeDieOpExperimentKD kd lhw is
  (Reroll rro cond lim) -> do
    limd <- range lim
    return $ DM.do
      limit <- limd
      let newDie = mkNewDie limit
      rolls <- is
      let (count, cutdownRolls) = countTriggers limit rolls
      if count == 0
        then DM.return cutdownRolls
        else (cutdownRolls ++) DM.<$> getDiceExperiment count newDie
    where
      mkNewDie limitValue
        | rro = die
        | otherwise = D.assuming (\i -> not $ applyCompare cond i limitValue) die
      countTriggers limitValue = foldr (\i ~(c, xs') -> if applyCompare cond i limitValue then (c + 1, xs') else (c, i : xs')) (0, [])

-- | Perform a keep/drop operation on the `Experiment` of dice rolls.
rangeDieOpExperimentKD :: (MonadException m) => KeepDrop -> LowHighWhere -> ExperimentList -> m ExperimentList
rangeDieOpExperimentKD kd (Where cond nb) is = do
  nbDis <- range nb
  return $ DM.do
    wherelimit <- nbDis
    filter (\i -> keepDrop $ applyCompare cond i wherelimit) DM.<$> is
  where
    keepDrop
      | kd == Keep = id
      | otherwise = not
rangeDieOpExperimentKD kd lhw is = do
  let nb = getValueLowHigh lhw
  case nb of
    Nothing -> whereException
    Just nb' -> do
      nbd <- range nb'
      return $ DM.do
        kdlh <- nbd
        (getKeep kdlh . sortBy') DM.<$> is
  where
    -- the below exception should never trigger - it is a hold over. it is
    -- present so that this thing type checks nicely.
    whereException = evaluationException "keep/drop where is unsupported" []
    order l l' = if isLow lhw then compare l l' else compare l' l
    sortBy' = sortBy order
    getKeep = if kd == Keep then genericTake else genericDrop

-- | Type class to get the overall range of a list of values.
--
-- Only used within `DiceStats` as I have no interest in producing statistics on
-- lists
class (ParseShow a) => RangeList a where
  -- | Try and get the `DistributionList` of the given value, throwing a
  -- `MonadException` on failure.
  rangeList :: (MonadException m, ParseShow a) => a -> m ExperimentList
  rangeList a = propagateException (parseShow a) (rangeList' a)

  rangeList' :: (MonadException m, ParseShow a) => a -> m ExperimentList

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
        getDiceExperiment valNum bd
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
