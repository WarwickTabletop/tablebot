-- |
-- Module      : Tablebot.Plugin.SmartCommandTH
-- Description : Automatic parser generation from function types.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- TemplateHaskell helpers for SmartCommands.
module Tablebot.Plugin.SmartCommandTH where

import Data.List (intersperse)
import Language.Haskell.TH

canParseInstances :: Int -> Q [Dec]
canParseInstances x = concat <$> mapM parseTupleInstances [2 .. x]

parseTupleInstances :: Int -> Q [Dec]
parseTupleInstances x
  | x <= 1 = pure []
  | otherwise = do
    pure [iDecl]
  where
    -- Type variable names t1..tn
    tvars :: [Name]
    tvars = [mkName ('t' : show n) | n <- [1 .. x]]

    -- Variable names v1..vn
    vars :: [Name]
    vars = [mkName ('v' : show n) | n <- [1 .. x]]

    -- The instance name, CanParse
    instName :: Name
    instName = mkName "CanParse"

    -- The n-tuple type (t1..tn).
    signature :: Type
    signature = foldl (\acc var -> AppT acc (VarT var)) (TupleT x) tvars

    -- The context of (CanParse t1, ... CanParse tn).
    cxt :: Cxt
    cxt = map (\var -> AppT (ConT instName) (VarT var)) tvars

    -- instance (CanParse t1... CanParse tn) => CanParse (t1, ... , tn) where
    iDecl :: Dec
    iDecl = InstanceD Nothing cxt (AppT (ConT instName) signature) [mDecl]

    -- pars = do
    --   x <- pars
    --   space
    --   ...
    --   return (x, ...)
    mDecl :: Dec
    mDecl = FunD (mkName "pars") [Clause [] body []]

    -- A do statement containing the relevant instructions.
    body :: Body
    body = NormalB . DoE $ intersperseSpace parseArgs ++ [returnArgs]

    -- Separate each parse with a space.
    intersperseSpace :: [Stmt] -> [Stmt]
    intersperseSpace = intersperse (NoBindS $ VarE (mkName "space"))

    -- Parse each type into its corresponding var.
    parseArgs :: [Stmt]
    parseArgs = zipWith genBind vars tvars

    -- Generates the pattern x <- pars @a.
    genBind :: Name -> Name -> Stmt
    genBind var ty = BindS (VarP var) (AppTypeE (VarE (mkName "pars")) (VarT ty))

    -- return (x, ...)
    returnArgs :: Stmt
    returnArgs = NoBindS $ AppE (VarE (mkName "return")) $ TupE $ map (Just . VarE) vars
