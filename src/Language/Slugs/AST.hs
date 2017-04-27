{-# LANGUAGE RecordWildCards #-}

module Language.Slugs.AST where

import Language.Slugs.Lens

import Data.Bits (Bits,testBit,bit)
import Data.List (foldl1')
import Data.Maybe (catMaybes)


data Var = VarBool String
         | VarNum String Int Int
           -- ^ Variable name, minimum and maximum integer values
           deriving (Show,Eq,Ord)

data Spec = Spec { specInput  :: [Var]
                 , specOutput :: [Var]
                 , specEnv    :: State
                 , specSys    :: State
                 } deriving (Show)

data State = State { stInit     :: Expr
                   , stTrans    :: Expr
                   , stLiveness :: [Expr]
                   } deriving (Show)

data Use = UVar Var
         | UNext Var
           deriving (Show,Eq,Ord)

data Expr = ENeg Expr
          | EAnd Expr Expr
          | EOr Expr Expr
          | EXor Expr Expr
          | EVar Use
          | EBit Use Int
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
traverseExpr _ e@EVar{}   = pure e
traverseExpr _ e@EBit{}   = pure e
traverseExpr _ ETrue      = pure ETrue
traverseExpr _ EFalse     = pure EFalse
traverseExpr f (EBuf es)  = EBuf <$> traverse f es
traverseExpr _ e@ERef{}   = pure e


-- AST Helpers -----------------------------------------------------------------

{-# SPECIALIZE numBits :: Int -> Int #-}
numBits :: Integral a => a -> Int
numBits n | n <= 0    = 1
          | otherwise = floor (logBase 2 (fromIntegral n)) + 1

class HasVar var where
  toUse      :: var -> Use
  toVar      :: var -> Var
  varBitSize :: var -> Int

instance HasVar Use where
  toUse = id
  {-# INLINE toUse #-}

  toVar (UVar  v) = v
  toVar (UNext v) = v
  {-# INLINE toVar #-}

  varBitSize (UVar  v) = varBitSize v
  varBitSize (UNext v) = varBitSize v
  {-# INLINE varBitSize #-}

instance HasVar Var where
  toUse = UVar
  {-# INLINE toUse #-}

  toVar = id
  {-# INLINE toVar #-}

  varBitSize (VarBool _)    = 1
  varBitSize (VarNum _ _ h) = numBits h
  {-# INLINE varBitSize #-}


{-# SPECIALIZE assignConst :: Var -> Int -> Expr #-}
assignConst :: (Bits a, Integral a, HasVar var) => var -> a -> Expr
assignConst var n = foldl1' EAnd [ genBit i | i <- [ 0 .. varBitSize var - 1 ] ]
  where
  genBit i | testBit n i = EBit use i
           | otherwise   = ENeg (EBit use i)

  use = toUse var


-- | Add variable limits for integer variables.
addLimits :: Spec -> Spec
addLimits Spec {..} =
  Spec { specEnv = addStateLimits envInitLimits envTransLimits specEnv
       , specSys = addStateLimits sysInitLimits sysTransLimits specSys
       , ..}

  where

  envInitLimits  = catMaybes [ varLimit (UVar  v) mn mx | v@(VarNum n mn mx) <- specInput  ]
  envTransLimits = catMaybes [ varLimit (UNext v) mn mx | v@(VarNum n mn mx) <- specInput  ]
  sysInitLimits  = catMaybes [ varLimit (UVar  v) mn mx | v@(VarNum n mn mx) <- specOutput ]
  sysTransLimits = catMaybes [ varLimit (UNext v) mn mx | v@(VarNum n mn mx) <- specOutput ]

  mkLimit ls | null ls   = ETrue
             | otherwise = foldl1' EAnd ls

  addStateLimits is ts State {..} =
    State { stInit  = stInit  `eAnd` mkLimit is
          , stTrans = stTrans `eAnd` mkLimit ts
          , ..}

-- | Produce a limit expression for a numeric variable.
varLimit :: Use    -- ^ Variable name
         -> Int    -- ^ Min value
         -> Int    -- ^ Max value
         -> Maybe Expr
varLimit var mn mx
  | mx == bit size - 1 = Nothing
  | otherwise          = Just (foldl step EFalse [0 .. size - 1])
  where
  size      = numBits mx
  limitDiff = mx + 1

  step acc n
    | testBit limitDiff n = EOr  (ENeg (EBit var n)) acc
    | otherwise           = EAnd (ENeg (EBit var n)) acc


implies :: Expr -> Expr -> Expr
implies a b = EOr (ENeg a) b

eAnd :: Expr -> Expr -> Expr
eAnd a b =
  let xs = elimEAnd a
      ys = elimEAnd b
   in case filter (/= ETrue) (xs ++ ys) of
        [] -> ETrue
        es -> foldl1 EAnd es 

-- | Eliminate nested conjunction.
elimEAnd :: Expr -> [Expr]
elimEAnd (EAnd a b) = elimEAnd a ++ elimEAnd b
elimEAnd e          = [e]
