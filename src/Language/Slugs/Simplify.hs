{-# LANGUAGE RecordWildCards #-}

module Language.Slugs.Simplify where

import Language.Slugs.AST
import Language.Slugs.Lens

import Control.Monad (guard)
import Data.List (group,sort)


-- Simplification --------------------------------------------------------------

simpSpec :: Spec -> Spec
simpSpec Spec { .. } =
  Spec { specEnv = simpState specEnv
       , specSys = simpState specSys
       , .. }

simpState :: State -> State
simpState State { .. } =
  State { stInit     = simpExpr stInit
        , stTrans    = simpExpr stTrans
        , stLiveness = map simpExpr stLiveness
        }

simpExpr :: Expr -> Expr
simpExpr  = rewriteOf traverseExpr simpExpr1

-- | Single-step simplification for slugs expressions.
simpExpr1 :: Expr -> Maybe Expr

simpExpr1 (ENeg ETrue)    = Just EFalse
simpExpr1 (ENeg EFalse)   = Just ETrue

simpExpr1 (EAnd EFalse _) = Just EFalse
simpExpr1 (EAnd _ EFalse) = Just EFalse

simpExpr1 (EOr ETrue _)   = Just ETrue
simpExpr1 (EOr _ ETrue)   = Just ETrue

simpExpr1 (ENeg (ENeg e)) = Just e

simpExpr1 (EAnd ETrue e)  = Just e
simpExpr1 (EAnd e ETrue)  = Just e

simpExpr1 (EOr EFalse e)  = Just e
simpExpr1 (EOr e EFalse)  = Just e

simpExpr1 (EXor EFalse e) = Just e
simpExpr1 (EXor e EFalse) = Just e

simpExpr1 (EXor ETrue e)  = Just (ENeg e)
simpExpr1 (EXor e ETrue)  = Just (ENeg e)

simpExpr1 (EXor (ENeg a) b) | a == b = Just ETrue
simpExpr1 (EXor a (ENeg b)) | a == b = Just ETrue

simpExpr1 (EXor a b) | a == b = Just EFalse

simpExpr1 e@EAnd{} =
  do let gs = group (sort (elimEAnd e))
     guard (any (> 1) (map length gs))
     return (foldl EAnd ETrue (map head gs))

simpExpr1 e@EOr{} =
  do let gs = group (sort (elimEAnd e))
     guard (any (> 1) (map length gs))
     return (foldl EOr ETrue (map head gs))

simpExpr1 _               = Nothing
