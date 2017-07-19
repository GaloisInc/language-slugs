{-# LANGUAGE RecordWildCards #-}

module Language.Slugs.PP where

import Language.Slugs.AST

import Text.PrettyPrint.HughesPJ (Doc,vcat,text,int,hsep,char,(<+>),(<>))


ppSpec :: Spec -> Doc
ppSpec Spec { .. } = vcat $ concat $
         [ [ section "INPUT"
           , vcat (map ppDecl specInput)
           , text " " ] | not (null specInput) ]

      ++ [ [ section "OUTPUT"
           , vcat (map ppDecl specOutput)
           , text " " ] | not (null specOutput) ]

      ++ [ppState "ENV" specEnv]
      ++ [ppState "SYS" specSys]

ppState :: String -> State -> [Doc]
ppState pfx State { .. } =
  [ section (pfx ++ "_INIT"),     ppTopExpr stInit,      text " "
  , section (pfx ++ "_TRANS"),    ppTopExpr stTrans,     text " "
  , section (pfx ++ "_LIVENESS"), ppLiveness stLiveness, text " " ]

section :: String -> Doc
section str = char '[' <> text str <> char ']'

ppDecl :: Var -> Doc
ppDecl (VarBool v)    = text v
ppDecl (VarNum v l h) =
  let var = text v
   in vcat (ppBit0 var l h : [ ppBitN var i | i <- [ 1 .. numBits h - 1 ] ])

ppBit0 :: Doc -> Int -> Int -> Doc
ppBit0 var l h = ppBitN var 0 <> char '.' <> int l <> char '.' <> int h

ppBitN :: Doc -> Int -> Doc
ppBitN var i = var <> char '@' <> int i


ppVar :: Var -> Doc
ppVar (VarBool v)    = text v
ppVar (VarNum v _ _) = text v

ppUse :: Use -> Doc
ppUse (UVar  v) = ppVar v
ppUse (UNext v) = ppVar v <> char '\''


ppTopExpr :: Expr -> Doc
ppTopExpr e = vcat (map ppExpr (elimEAnd e))

ppLiveness :: [Expr] -> Doc
ppLiveness [] = ppExpr ETrue
ppLiveness es = vcat (map ppExpr es)

ppExpr :: Expr -> Doc
ppExpr (ENeg e)   = char '!' <+> ppExpr e
ppExpr (EAnd a b) = char '&' <+> ppExpr a <+> ppExpr b
ppExpr (EOr a b)  = char '|' <+> ppExpr a <+> ppExpr b
ppExpr (EXor a b) = char '^' <+> ppExpr a <+> ppExpr b
ppExpr (EVar v)   = ppUse v

ppExpr (EBit (UNext v) i) = ppEBit v i <> char '\''
ppExpr (EBit (UVar  v) i) = ppEBit v i

ppExpr ETrue      = char '1'
ppExpr EFalse     = char '0'
ppExpr (EBuf es)  = char '$' <+> int (length es) <+> hsep (map ppExpr es)
ppExpr (ERef n)   = char '?' <+> int n


ppEBit :: Var -> Int -> Doc

ppEBit var@(VarNum _ l h) i
  | i == 0    = ppBit0 (ppVar var) l h
  | otherwise = ppBitN (ppVar var) i

ppEBit VarBool{} _ = error "EBit used with boolean variable"
