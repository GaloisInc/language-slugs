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

      ++ ppState "ENV" specEnv
      ++ ppState "SYS" specSys

ppState :: String -> State -> [[Doc]]
ppState pfx State { .. } =
     [ [ section (pfx ++ "_INIT"),     ppTopExpr i, text " " ] | Just i <- [stInit    ] ]
  ++ [ [ section (pfx ++ "_TRANS"),    ppTopExpr t, text " " ] | Just t <- [stTrans   ] ]
  ++ [ [ section (pfx ++ "_LIVENESS"), ppTopExpr t, text " " ] | Just t <- [stLiveness] ]

section :: String -> Doc
section str = char '[' <> text str <> char ']'

ppDecl :: Var -> Doc
ppDecl (VarBool v)    = text v
ppDecl (VarNum v l h) =
   vcat (ppBit0 v l h : [ ppBitN v i | i <- [ 1 .. numBits h - 1 ] ])

ppBit0 :: String -> Int -> Int -> Doc
ppBit0 v l h = ppBitN v 0 <> char '.' <> int l <> char '.' <> int h

ppBitN :: String -> Int -> Doc
ppBitN v i = text v <> char '@' <> int i


ppVar :: Var -> Doc
ppVar (VarBool v)    = text v
ppVar (VarNum v _ _) = text v


ppTopExpr :: Expr -> Doc
ppTopExpr e = vcat (map ppExpr (elimEAnd e))


ppExpr :: Expr -> Doc
ppExpr (ENeg e)   = char '!' <+> ppExpr e
ppExpr (EAnd a b) = char '&' <+> ppExpr a <+> ppExpr b
ppExpr (EOr a b)  = char '|' <+> ppExpr a <+> ppExpr b
ppExpr (EXor a b) = char '^' <+> ppExpr a <+> ppExpr b
ppExpr (ENext v)  = ppVar v <> char '\''
ppExpr (EVar v)   = ppVar v

ppExpr (EBit v i) =
  case v of
    VarNum s l h | i == 0    -> ppBit0 s l h
                 | otherwise -> ppBitN s i
    VarBool _    -> error "EBit used with boolean variable"

ppExpr ETrue      = char '1'
ppExpr EFalse     = char '0'
ppExpr (EBuf es)  = char '$' <+> int (length es) <+> hsep (map ppExpr es)
ppExpr (ERef n)   = char '?' <+> int n
