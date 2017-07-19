{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Slugs.CSE (cse) where

import Language.Slugs.AST

import qualified Data.Map.Strict    as Map
import qualified Data.IntMap.Strict as IntMap
import           MonadLib (Id,runId,StateT,StateM(..),runStateT)


-- | Rewrite one expression into one that attempts to share common
-- sub-expressions through the use of memory buffers.
cse :: Expr -> Expr
cse e = fst (runId (runStateT emptyRW (unCSE (mkMemory e))))


newtype CSE a = CSE { unCSE :: StateT RW Id a
                    } deriving (Functor,Applicative,Monad)

data RW = RW { rwExprs :: !(BiMap Expr)
             } deriving (Show)

emptyRW :: RW
emptyRW  = RW { rwExprs = emptyBiMap }

-- | Swap out the current expression environment for a new one.
swapExprs :: BiMap Expr -> CSE (BiMap Expr)
swapExprs es = CSE $
  do RW { .. } <- get
     set $! RW { rwExprs = es, .. }
     pure rwExprs

-- | Make a memory buffer for common expressions in the given expression.
mkMemory :: Expr -> CSE Expr
mkMemory e =
  do es   <- swapExprs emptyBiMap
     e'   <- hashCons e
     deps <- swapExprs es

     let size = sizeBiMap deps
     if size == 0
        then return e'
        else case e' of
               -- catch the case where the value of the whole expression is just
               -- a reference to the most recently consumed part (sort of a
               -- tail-call optimization)
               ERef n | n == size - 1 -> return (EBuf (elemsBiMap deps))
               _                      -> return (EBuf (elemsBiMap deps ++ [e']))


hashCons :: Expr -> CSE Expr

hashCons ETrue    = pure ETrue
hashCons EFalse   = pure EFalse
hashCons e@EBit{} = pure e

hashCons (ENeg e) =
  do ref <- hashCons e
     hash (ENeg ref)

hashCons (EAnd a b) =
  do refA <- hashCons a
     refB <- hashCons b
     hash (EAnd refA refB)

hashCons x = error (show x)

-- data Expr = ENeg Expr
--           | EAnd Expr Expr
--           | EOr Expr Expr
--           | EXor Expr Expr
--           | ENext Var
--           | EVar Var
--           | EBit Var Int
--           | ETrue
--           | EFalse
--           | EBuf [Expr]
--             -- ^ Memory buffers
--           | ERef Int

-- | Insert elements into the BiMap, and return a lookup expression into the
-- current memory.
hash :: Expr -> CSE Expr
hash ETrue  = pure ETrue
hash EFalse = pure EFalse
hash e      = CSE $
  do RW { .. } <- get
     let (i,m') = insertBiMap e rwExprs
     set $! RW { rwExprs = m', .. }
     pure (ERef i)


-- Utility ---------------------------------------------------------------------

data BiMap a = BiMap { bKeys :: !(Map.Map a Int)
                     , bVals :: !(IntMap.IntMap a)
                     , bNext :: !Int
                     } deriving (Show)

emptyBiMap :: BiMap a
emptyBiMap  = BiMap { bKeys = Map.empty, bVals = IntMap.empty, bNext = 0 }

insertBiMap :: Ord a => a -> BiMap a -> (Int,BiMap a)
insertBiMap a m@BiMap { .. }
  | Just i <- Map.lookup a bKeys = (i, m)
  | otherwise                    = (bNext , BiMap { bKeys = Map.insert a bNext bKeys
                                                  , bVals = IntMap.insert bNext a bVals
                                                  , bNext = bNext + 1 })

sizeBiMap :: BiMap a -> Int
sizeBiMap BiMap { .. } = bNext

elemsBiMap :: BiMap a -> [a]
elemsBiMap BiMap { .. } = IntMap.elems bVals
