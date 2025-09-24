{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Dice.RoundtripSpec where

import Test.Hspec
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec.Hedgehog ()
import Data.Kind
import Text.Read
import Tablebot.Plugins.Roll.Dice.DiceParsing
import Tablebot.Plugins.Roll.Dice.DiceFunctions
import Tablebot.Utility.SmartParser.SmartParser
import Tablebot.Utility.Parser
import Text.Megaparsec (runParser, eof)
import Tablebot.Plugins.Roll.Dice.DiceData as Dice
import qualified Data.Text as T
import Control.Monad.IO.Class

genExpr :: MonadGen m => m Expr
genExpr =
  Gen.recursive Gen.choice [Expr <$> genBin genTerm] [Gen.subtermM genExpr $ \expr -> ExprMisc <$> genMisc False (pure expr)]

genMisc :: MonadGen m => Bool -> m a -> m (MiscData a)
genMisc isList genA = Gen.choice [MiscIf <$> genIf genA, MiscVar <$> genVar isList genA]

genIf :: MonadGen m => m a -> m (If a)
genIf genA = If <$> genExpr <*> genA <*> genA

genVarName :: MonadGen m => m T.Text
genVarName = Gen.filterT (\var -> not $ any (`T.isPrefixOf` var) (integerFunctionsList <> listFunctionsList))
  $ Gen.text (Range.linear 3 10) (Gen.element ['a'..'z'])

genVar :: MonadGen m => Bool -> m a -> m (Dice.Var a)
genVar isList genA =
    Dice.Var
      <$> ((if isList then ("l_" <>) else id) <$> genVarName)
      <*> genA

genBin :: (MonadGen m, Enum typ, Bounded typ) => m a -> m (BinOp a typ)
genBin genA =
  let opCount = Range.exponential 0 4
  in BinOp
    <$> genA
    <*> Gen.list opCount ((,) <$> Gen.element [minBound..maxBound] <*> genA)

genTerm :: MonadGen m => m Term
genTerm = Term <$> genBin genNeg

genNeg :: MonadGen m => m Negation
genNeg = Gen.frequency [(2, NoNeg <$> genExpo), (1, Neg <$> genExpo)]

genExpo :: MonadGen m => m Expo
genExpo = 
  Gen.recursive Gen.choice [NoExpo <$> genFunc] [Gen.subtermM genExpo (\expo -> (`Expo` expo) <$> genFunc )]

genFunc :: MonadGen m => m Func
genFunc = Gen.frequency
  [ (5, NoFunc <$> genBase)
  -- , (1, Func <$> Gen.element integerFunctions <*> Gen.list (Range.linear 1 2) genArg)
  ]

genArg :: MonadGen m => m ArgValue
genArg = Gen.choice [AVExpr <$> genExpr, AVListValues <$> genListValues]

genBase :: MonadGen m => m Base
genBase = Gen.frequency [(2, NBase <$> genNumBase), (2, DiceBase <$> genDice), (1, NumVar <$> genVarName)]

genNumBase :: MonadGen m => m NumBase
genNumBase = 
  Gen.recursive Gen.choice [Value <$> Gen.integral (Range.linear 0 100)] [NBParen . Paren <$> genExpr]

genDice :: MonadGen m => m Dice
genDice = Dice <$> genNumBase <*> genDie <*> Gen.list (Range.exponential 0 3) genDieOpOption

genDie :: MonadGen m => m Die
genDie = Gen.frequency (fmap (fmap (fmap MkDie)) strictDie <> [(1, MkDie . LazyDie <$> Gen.frequency strictDie)])
  where
  strictDie :: MonadGen m => [(Int, m (DieOf 'Strict))]
  strictDie = [(3, Die <$> genNumBase), (1, CustomDie <$> genListValuesBase)]

genDieOpOption :: MonadGen m => m DieOpOption
genDieOpOption = Gen.choice (fmap (fmap MkDieOpOption) strictDieOp <> [MkDieOpOption . DieOpOptionLazy <$> Gen.choice strictDieOp])
  where
  strictDieOp :: MonadGen m => [m (DieOpOptionOf 'Strict)]
  strictDieOp = 
    [ DieOpOptionKD <$> Gen.element [Keep, Drop] <*> Gen.frequency
      [ (2, Gen.element [Low, High] <*> genNumBase)
      , (1, Where <$> genAdvancedOrdering <*> genNumBase)
      ]
    , Reroll <$> Gen.element [True, False] <*> genAdvancedOrdering <*> genNumBase
    ]

genAdvancedOrdering :: MonadGen m => m AdvancedOrdering
genAdvancedOrdering = Gen.element $ fst advancedOrderingMapping

genListValuesBase :: MonadGen m => m ListValuesBase
genListValuesBase = Gen.choice
  [ LVBList <$> Gen.list (Range.exponential 1 10) genExpr
  , LVBParen . Paren <$> genListValues
  ]

genListValues :: MonadGen m => m ListValues
genListValues = Gen.frequency
  [ (4, MultipleValues <$> genNumBase <*> genBase)
  , (2, LVBase <$> genListValuesBase)
  , (2, ListValuesMisc <$> genMisc True genListValues)
  -- , (1, LVFunc <$> Gen.element listFunctions <*> Gen.list (Range.linear 1 2) genArg)
  , (1, LVVar . ("l_" <>) <$> genVarName)
  ]

spec_roundtrip_dice :: Spec
spec_roundtrip_dice = do
  it "roundtrip dice" $ do
    dice <- forAll genExpr :: PropertyT IO Expr
    liftIO $ print (parseShow dice)
    Right dice === runParser (pars <* eof) "" (parseShow dice)

