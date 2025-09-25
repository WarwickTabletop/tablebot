{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Tablebot.Plugins.Roll.Dice.DiceData
-- Description : Data structures for dice and other expressions.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This plugin contains the basics for dice expressions and values. For an
-- overview of how they all fit together, you can view
-- `Tablebot.Plugins.Roll.Dice` to see the regex for this and brief explanations
-- for each data type.
module Tablebot.Plugins.Roll.Dice.DiceData where

import Data.Map as M (Map, fromList)
import Data.Text (Text)
import Data.Tuple (swap)
import Tablebot.Plugins.Roll.Dice.DiceFunctions (FuncInfo, FuncInfoBase)

-- | Set the variable `varName` to the value `varValue`. This also returns the
-- evaluated `varValue`.
--
-- List variables have to be prefixed with `l_`. This really helps with parsing.
data Var a = Var {varName :: Text, varValue :: a} | VarLazy {varName :: Text, varValue :: a} deriving (Show, Eq)

-- | If the first value is truthy (non-zero or a non-empty list) then return
-- the `thenValue`, else return the `elseValue`.
data If b = If {ifCond :: Expr, thenValue :: b, elseValue :: b} deriving (Show, Eq)

-- | Either an If or a Var that returns a `b`.
data MiscData b = MiscIf (If b) | MiscVar (Var b) deriving (Show, Eq)

-- | An expression is just an Expr or a ListValues with a semicolon on the end.
--
-- When evaluating, VarLazy expressions are handled with a special case - they
-- are not evaluated until the value is first referenced. Otherwise, the value
-- is evaluated as the statement is encountered
data Statement = StatementExpr Expr | StatementListValues ListValues deriving (Show, Eq)

-- | A program is a series of `Statement`s followed by either a `ListValues` or
-- an Expr.
data Program = Program [Statement] (Either ListValues Expr) deriving (Show, Eq)

-- | The value of an argument given to a function.
data ArgValue = AVExpr Expr | AVListValues ListValues
  deriving (Show, Eq)

-- | The type for list values.
data ListValues
  = -- | Represents `N#B`, where N is a NumBase (numbers, parentheses) and B is a Base (numbase or dice value)
    MultipleValues NumBase Base
  | -- | Represents a function call with the given arguments
    LVFunc (FuncInfoBase [Integer]) [ArgValue]
  | -- | A base ListValues value - parentheses or a list of expressions
    LVBase ListValuesBase
  | -- | A variable that has been defined elsewhere.
    LVVar Text
  | -- | A misc list values expression.
    ListValuesMisc (MiscData ListValues)
  deriving (Show, Eq)

-- | The type for basic list values (that can be used as is for custom dice).
--
-- A basic list value can be understood as one that is indivisible, and/or
-- atomic. They represent either a list value in parentheses, or a list of
-- expressions. Effectively what this is used for is so that these can be used
-- as dice side values.
data ListValuesBase = LVBParen (Paren ListValues) | LVBList [Expr]
  deriving (Show, Eq)

-- | The type for a binary operator between one or more `sub` values
data BinOp sub typ = BinOp sub [(typ, sub)]
  deriving (Show, Eq)

-- | Convenience pattern for the empty list.
pattern SingBinOp :: (Operation typ) => sub -> BinOp sub typ
pattern SingBinOp a <-
  BinOp a []
  where
    SingBinOp a = BinOp a []

-- | The type class that means we can get an operation on integers from a value.
class Operation a where
  getOperation :: a -> (forall n. (Integral n) => n -> n -> n)

-- | The type of the top level expression.
--
-- Represents either a misc expression or additive operations between terms.
data Expr = ExprMisc (MiscData Expr) | Expr (BinOp Term ExprType)
  deriving (Show, Eq)

-- | The type of the additive expression, either addition or subtraction.
data ExprType = Add | Sub
  deriving (Show, Eq, Enum, Bounded)

instance Operation ExprType where
  getOperation Sub = (-)
  getOperation Add = (+)

-- | Represents multiplicative operations between (possible) negations.
newtype Term = Term (BinOp Negation TermType)
  deriving (Show, Eq)

-- | The type of the additive expression, either addition or subtraction.
data TermType = Multi | Div
  deriving (Show, Eq, Enum, Bounded)

instance Operation TermType where
  getOperation Multi = (*)
  getOperation Div = div

-- | The type representing a possibly negated value.
data Negation = Neg Expo | NoNeg Expo
  deriving (Show, Eq)

-- | The type representing a value with exponentials.
data Expo = Expo Func Expo | NoExpo Func
  deriving (Show, Eq)

-- | The type representing a single function application, or a base item.
data Func = Func FuncInfo [ArgValue] | NoFunc Base
  deriving (Show, Eq)

-- | The type representing an integer value or an expression in brackets.
data NumBase = NBParen (Paren Expr) | Value Integer
  deriving (Show, Eq)

-- | Container for a parenthesised value.
newtype Paren a = Paren a
  deriving (Show, Eq)

-- | The type representing a numeric base value value or a dice value.
data Base = NBase NumBase | DiceBase Dice | NumVar Text
  deriving (Show, Eq)

-- Dice Operations after this point

data Laziness = Lazy | Strict

-- | The type representing a simple N sided die or a custom die, or a lazy one
-- of one of those values.
data DieOf (l :: Laziness) where
  Die :: NumBase -> DieOf l
  CustomDie :: ListValuesBase -> DieOf l
  LazyDie :: DieOf Strict -> DieOf Lazy

deriving instance Show (DieOf l)
deriving instance Eq (DieOf l)

data Die where
  MkDie :: DieOf l -> Die

deriving instance Show Die
instance Eq Die where
  (==) (MkDie die1) (MkDie die2) = case (die1, die2) of
    (Die n1, Die n2) -> n1 == n2
    (CustomDie lvb1, CustomDie lvb2) -> lvb1 == lvb2
    (LazyDie do1, LazyDie do2) -> do1 == do2
    _ -> False

-- | The type representing a number of dice equal to the `Base` value, and
-- possibly some die options.
data Dice = Dice NumBase Die [DieOpOption]
  deriving (Show, Eq)

-- | Some more advanced ordering options for things like `<=` and `/=`.
data AdvancedOrdering = Not AdvancedOrdering | OrderingId Ordering | And [AdvancedOrdering] | Or [AdvancedOrdering]
  deriving (Show, Eq, Ord)

-- | Compare two values according an advanced ordering.
applyCompare :: (Ord a) => AdvancedOrdering -> a -> a -> Bool
applyCompare (OrderingId o) a b = o == compare a b
applyCompare (And os) a b = all (\o -> applyCompare o a b) os
applyCompare (Or os) a b = any (\o -> applyCompare o a b) os
applyCompare (Not o) a b = not (applyCompare o a b)

-- | Create a mapping between a Text of the advanced ordering, and vice versa.
advancedOrderingMapping :: (Map Text AdvancedOrdering, Map AdvancedOrdering Text)
advancedOrderingMapping = (M.fromList lst, M.fromList $ swap <$> lst)
  where
    lst =
      [ ("/=", Not (OrderingId EQ)),
        ("<=", Or [OrderingId EQ, OrderingId LT]),
        (">=", Or [OrderingId EQ, OrderingId GT]),
        ("<", OrderingId LT),
        ("=", OrderingId EQ),
        (">", OrderingId GT)
      ]

-- | The type representing a die option; a reroll, a keep/drop operation, or
-- lazily performing some other die option.
data DieOpOptionOf (l :: Laziness) where
  Reroll :: {rerollOnce :: Bool, condition :: AdvancedOrdering, limit :: NumBase}
    -> DieOpOptionOf l
  DieOpOptionKD :: KeepDrop -> LowHighWhere -> DieOpOptionOf l
  DieOpOptionLazy :: DieOpOptionOf Strict -> DieOpOptionOf Lazy

deriving instance Show (DieOpOptionOf l)
deriving instance Eq (DieOpOptionOf l)

data DieOpOption where
  MkDieOpOption :: DieOpOptionOf l -> DieOpOption

deriving instance Show DieOpOption
instance Eq DieOpOption where
  (==) (MkDieOpOption doo1) (MkDieOpOption doo2) = case (doo1, doo2) of
    (Reroll rro1 cond1 lim1, Reroll rro2 cond2 lim2) ->
      rro1 == rro2 && cond1 == cond2 && lim1 == lim2
    (DieOpOptionKD kd1 lhw1, DieOpOptionKD kd2 lhw2) -> kd1 == kd2 && lhw1 == lhw2
    (DieOpOptionLazy dooo1, DieOpOptionLazy dooo2) -> dooo1 == dooo2
    _ -> False

data LowHigh = Low | High
  deriving (Show, Eq, Enum, Bounded)

-- | A type used to designate how the keep/drop option should work
data LowHighWhere = LH LowHigh NumBase | Where AdvancedOrdering NumBase deriving (Show, Eq)

-- | Utility function to get the integer determining how many values to get
-- given a `LowHighWhere`. If the given value is `Low` or `High`, then Just the
-- NumBase contained is returned. Else, Nothing is returned.
getValueLowHigh :: LowHighWhere -> Maybe NumBase
getValueLowHigh (LH _ i) = Just i
getValueLowHigh (Where _ _) = Nothing

-- | Returns whether the given `LowHighWhere` is `Low` or not.
isLow :: LowHighWhere -> Bool
isLow (LH Low _) = True
isLow _ = False

-- | Utility value for whether to keep or drop values.
data KeepDrop = Keep | Drop deriving (Show, Eq, Enum, Bounded)

-- | Utility type class for quickly promoting values.
class Converter a b where
  -- | Function that promotes an element of type `a` to an element of type `b`.
  promote :: a -> b

instance Converter ListValuesBase ListValues where
  promote = LVBase

instance (Converter a sub, Operation typ) => Converter a (BinOp sub typ) where
  promote = SingBinOp . promote

instance (Converter a Term) => Converter a Expr where
  promote = Expr . promote

instance (Converter a Negation) => Converter a Term where
  promote = Term . promote

instance (Converter a Expo) => Converter a Negation where
  promote = NoNeg . promote

instance (Converter a Func) => Converter a Expo where
  promote = NoExpo . promote

instance (Converter a Base) => Converter a Func where
  promote = NoFunc . promote

instance Converter Integer Base where
  promote = NBase . promote

instance Converter NumBase Base where
  promote = NBase

instance Converter Integer NumBase where
  promote = Value

instance Converter Dice Base where
  promote = DiceBase

instance Converter Die Base where
  promote d = promote $ Dice (promote (1 :: Integer)) d []

instance Converter [Integer] ListValues where
  promote = LVBase . LVBList . (promote <$>)
