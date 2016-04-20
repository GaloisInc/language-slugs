{-# LANGUAGE RecordWildCards #-}

module Language.Slugs.AST where

import Language.Slugs.Lens

import Data.Bits (Bits,testBit)
import Data.List (foldl1')


data Var = VarBool String
         | VarNum String Int Int
           -- ^ Variable name, minimum and maximum integer values
           deriving (Show,Eq,Ord)

data Decl = DeclVar String
          | DeclNum String Int Int
            deriving (Show,Eq)

data Spec = Spec { specInput  :: [Decl]
                 , specOutput :: [Decl]
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
          | EBit Var Int
          | ETrue
          | EFalse
          | EBuf [Expr]
            -- ^ Memory buffers
          | ERef Int
            -- ^ Memory reference, must be in the context of a buffer
            deriving (Show,Eq,Ord)

-- | Child traversal for expressions.
traverseExpr :: Traversal' Expr Expr
traverseExpr f (ENeg e)   = ENeg <$> f e
traverseExpr f (EAnd a b) = EAnd <$> f a <*> f b
traverseExpr f (EOr  a b) = EOr  <$> f a <*> f b
traverseExpr f (EXor a b) = EXor <$> f a <*> f b
traverseExpr _ e@ENext{}  = pure e
traverseExpr _ e@EVar{}   = pure e
traverseExpr _ e@EBit{}   = pure e
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


-- AST Helpers -----------------------------------------------------------------

{-# SPECIALIZE numBits :: Int -> Int #-}
numBits :: Integral a => a -> Int
numBits n = floor (logBase 2 (fromIntegral n)) + 1

{-# SPECIALIZE assignConst :: Var -> Int -> Expr #-}
assignConst :: (Bits a, Integral a) => Var -> a -> Expr
assignConst v n = foldl1' EAnd [ genBit i | i <- [ 0 .. varBitSize v - 1 ] ]
  where
  genBit i | testBit n i = EBit v i
           | otherwise   = ENeg (EBit v i)

varBitSize :: Var -> Int
varBitSize (VarNum _ _ h) = numBits h
varBitSize (VarBool _)    = error "varBitSize: not a bit vector"

implies :: Expr -> Expr -> Expr
implies a b = EOr (ENeg a) b

-- | Eliminate nested conjunction.
elimEAnd :: Expr -> [Expr]
elimEAnd (EAnd a b) = elimEAnd a ++ elimEAnd b
elimEAnd e          = [e]
