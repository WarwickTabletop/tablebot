module Dice.RoundtripSpec where

import Dice.Gen
import Hedgehog
import Tablebot.Plugins.Roll.Dice.DiceData as Dice
import Tablebot.Plugins.Roll.Dice.DiceParsing ()
import Tablebot.Utility.Parser
import Tablebot.Utility.SmartParser.SmartParser
import Test.Hspec
import Test.Hspec.Hedgehog ()
import Text.Megaparsec (eof, runParser)

spec_roundtrip_dice :: Spec
spec_roundtrip_dice = do
  it "roundtrip dice" $ do
    dice <- forAll genExpr :: PropertyT IO Expr
    Right dice === runParser (pars <* eof) "" (parseShow dice)
