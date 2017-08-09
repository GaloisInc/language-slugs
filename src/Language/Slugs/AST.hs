{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ParallelListComp #-}

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


skipBuffers :: Traversal' Expr Expr
skipBuffers _ e@EBuf{} = pure e
skipBuffers f e        = traverseExpr f e


-- | Modify all references underneath this expression.
modifyRefs :: (Int -> Int) -> (Expr -> Expr)
modifyRefs modify = transformOf skipBuffers $ \ e ->
  case e of
    ERef i -> ERef $! modify i
    _      -> e


-- AST Helpers -----------------------------------------------------------------

{-# SPECIALIZE numBits :: Int -> Int #-}
numBits :: Integral a => a -> Int
numBits n | n <= 0    = 0
          | otherwise = floor (logBase 2 (fromIntegral n) :: Double) + 1


class Atom expr where
  -- | The number of bits required by this sub-expression.
  bitSize :: expr -> Int

  -- | Project the nth bit from the expression.
  projBit :: expr -> Int -> Expr

  -- | Convert to an @Expr@
  toExpr :: expr -> Expr

instance Atom Integer where
  bitSize n = max (numBits n) 0

  projBit n i | testBit n i = ETrue
              | otherwise   = EFalse

  -- XXX: should we even try to convert an integer literal to an expression?
  toExpr _ = undefined

instance Atom Var where
  bitSize (VarBool _)    = 1
  bitSize (VarNum _ _ h) = numBits h
  {-# INLINE bitSize #-}

  projBit var = projBit (toUse var)
  {-# INLINE projBit #-}

  toExpr v = EVar (UVar v)
  {-# INLINE toExpr #-}

instance Atom Use where
  bitSize (UVar  v) = bitSize v
  bitSize (UNext v) = bitSize v
  {-# INLINE bitSize #-}

  projBit u n
    | n < bitSize u = case toVar u of
                        VarBool{} -> EVar u
                        VarNum{}  -> EBit u n
    | otherwise     = error "Invalid bit projection"

  toExpr = EVar
  {-# INLINE toExpr #-}


class Atom var => HasVar var where
  toUse :: var -> Use
  toVar :: var -> Var

instance HasVar Use where
  toUse = id
  {-# INLINE toUse #-}

  toVar (UVar  v) = v
  toVar (UNext v) = v
  {-# INLINE toVar #-}


instance HasVar Var where
  toUse = UVar
  {-# INLINE toUse #-}

  toVar = id
  {-# INLINE toVar #-}


-- | Get a list of bits from an atomic expression.
atomBits :: Atom var => var -> [Expr]
atomBits var = [ projBit var i | i <- [ 0 .. bitSize var - 1 ] ]

-- | Pad the bits of an atomic expression to a known bit length.
--
-- prop> length (padBits var n) == max n 0
padBits :: Atom var => var -> Int -> [Expr]
padBits var n
  | n < width = take n bits
  | otherwise = bits ++ replicate (n - width) EFalse
  where
  width = bitSize var
  bits  = [ projBit var i | i <- [ 0 .. width - 1 ] ]


-- | Add variable limits for integer variables.
addLimits :: Spec -> Spec
addLimits Spec {..} =
  Spec { specEnv = addStateLimits envInitLimits envTransLimits specEnv
       , specSys = addStateLimits sysInitLimits sysTransLimits specSys
       , ..}

  where

  envInitLimits  = catMaybes [ varLimit (UVar  v) mn mx | v@(VarNum _ mn mx) <- specInput  ]
  envTransLimits = catMaybes [ varLimit (UNext v) mn mx | v@(VarNum _ mn mx) <- specInput  ]
  sysInitLimits  = catMaybes [ varLimit (UVar  v) mn mx | v@(VarNum _ mn mx) <- specOutput ]
  sysTransLimits = catMaybes [ varLimit (UNext v) mn mx | v@(VarNum _ mn mx) <- specOutput ]

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
varLimit var _ mx
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


-- Numeric Operations ----------------------------------------------------------

assignConst :: (Bits a, Integral a, HasVar var) => var -> a -> Expr
assignConst  = eqConst

{-# SPECIALIZE eqConst :: Var -> Int -> Expr #-}
eqConst :: (Bits a, Integral a, HasVar var) => var -> a -> Expr
eqConst var n = foldl1' EAnd [ genBit i | i <- [ 0 .. bitSize var - 1 ] ]
  where
  genBit i | testBit n i = EBit use i
           | otherwise   = ENeg (EBit use i)

  use = toUse var


mkBuffer :: [Expr] -> Int -> Expr
mkBuffer es result = EBuf (es ++ [ ERef result ])


-- | Generate the elements of a memory buffer that will implement the addition
-- operation, as well as a list of expressions that select the bits that make up
-- the result, and the final carry bit, which is true if the addition overflows.
add :: (Atom a, Atom b) => a -> b -> ([Expr],[Expr],Expr)
add a b = add' (atomBits a) (atomBits b)

-- | Generate the elements of a memory buffer that will implement the addition
-- operation, as well as a list of expressions that select the bits that make up
-- the result, and the final carry bit, which is true if the addition overflows.
--
-- NOTE: this is translated from the `structuredslugs` compiler,
-- https://github.com/VerifiableRobotics/slugs/blob/master/tools/StructuredSlugsParser/Parser.py
add' :: [Expr] -> [Expr] -> ([Expr],[Expr],Expr)
add' as bs
  | null as   = ([], bs, EFalse)
  | null bs   = ([], as, EFalse)
  | otherwise = (eqbits ++ overflow, result, lastCarry)
  where

  asLen = length as
  bsLen = length bs

  (x0:xs,y0:ys) | asLen < bsLen = (as,bs)
                | otherwise     = (bs,as)

  s0 = x0 `EXor` y0
  c0 = x0 `EAnd` y0

  -- The parts where the two lists are the same length
  -- NOTE: in the resulting buffer, sum and carry bits are interleaved
  eqbits = s0
         : c0
         : concat [ [ sumBit x y cin, carryBit x y cin ]
                  | i <- [ 1,3 .. ], let cin = ERef i
                  | x <- xs
                  | y <- ys ]

  eqlen = length eqbits
  xsLen = length xs

  -- the parts of ys that overflow
  overflow = concat [ [ y `EXor` cin, y `EAnd` cin ]
                    | i <- [ eqlen, eqlen + 2 .. ], let cin  = ERef (i - 1)
                    | y <- drop xsLen ys ]

  sumBit   x y cin = x `EXor` y `EXor` cin
  carryBit x y cin = (x `EAnd` y) `EOr` ((x `EXor` y) `EAnd` cin)

  answerBits = max asLen bsLen

  result = [ ERef (i * 2) | i <- [ 0 .. answerBits - 1 ] ]

  lastCarry = ERef (2 * answerBits)
