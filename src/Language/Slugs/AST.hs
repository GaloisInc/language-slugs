{-# LANGUAGE RecordWildCards #-}

module Language.Slugs.AST where

import Language.Slugs.Lens


type Var = String

data Spec = Spec { specInput  :: [Var]
                 , specOutput :: [Var]
                 , specEnv    :: State
                 , specSys    :: State
                 } deriving (Show)

data State = State { stInit     :: Expr
                   , stTrans    :: Expr
                   , stLiveness :: Expr
                   } deriving (Show)

data Expr = ENeg Expr
          | EAnd Expr Expr
          | EOr Expr Expr
          | EXor Expr Expr
          | ENext Var
          | EVar Var
          | ETrue
          | EFalse
          | EBuf [Expr]
            -- ^ Memory buffers
          | ERef Int
            -- ^ Memory reference, must be in the context of a buffer
            deriving (Show,Eq)

-- | Child traversal for expressions.
traverseExpr :: Traversal' Expr Expr
traverseExpr f (ENeg e)   = ENeg <$> f e
traverseExpr f (EAnd a b) = EAnd <$> f a <*> f b
traverseExpr f (EOr  a b) = EOr  <$> f a <*> f b
traverseExpr f (EXor a b) = EXor <$> f a <*> f b
traverseExpr _ e@ENext{}  = pure e
traverseExpr _ e@EVar{}   = pure e
traverseExpr _ ETrue      = pure ETrue
traverseExpr _ EFalse     = pure EFalse
traverseExpr f (EBuf es)  = EBuf <$> traverse f es
traverseExpr _ e@ERef{}   = pure e

-- | Substitution for variables.
subst :: [(Var,Expr)] -> Expr -> Expr
subst env = rewriteOf traverseExpr f
  where
  f (EVar v) = lookup v env
  f _        = Nothing
