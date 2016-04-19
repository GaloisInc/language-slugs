{-# LANGUAGE RecordWildCards #-}

module Language.Slugs.PP where

import Language.Slugs.AST

import Text.PrettyPrint.HughesPJ (Doc,vcat,text,int,hsep,char,(<+>),(<>))


ppSpec :: Spec -> Doc
ppSpec Spec { .. } =
  vcat [ text "INPUT"
       , vcat (map ppVar specInput)

       , text "OUTPUT"
       , vcat (map ppVar specOutput)

       , text "ENV_INIT"
       , ppExpr (stInit specEnv)

       , text "ENV_TRANS"
       , ppExpr (stTrans specEnv)

       , text "ENV_LIVELINESS"
       , ppExpr (stLiveness specEnv)

       , text "SYS_INIT"
       , ppExpr (stInit specSys)

       , text "SYS_TRANS"
       , ppExpr (stTrans specSys)

       , text "SYS_LIVELINESS"
       , ppExpr (stLiveness specSys)
       ]


ppVar :: Var -> Doc
ppVar  = text


ppExpr :: Expr -> Doc
ppExpr (ENeg e)   = char '!' <+> ppExpr e
ppExpr (EAnd a b) = char '&' <+> ppExpr a <+> ppExpr b
ppExpr (EOr a b)  = char '|' <+> ppExpr a <+> ppExpr b
ppExpr (EXor a b) = char '^' <+> ppExpr a <+> ppExpr b
ppExpr (ENext v)  = ppVar v <> char '\''
ppExpr (EVar v)   = ppVar v
ppExpr ETrue      = char '1'
ppExpr EFalse     = char '0'
ppExpr (EBuf es)  = char '$' <+> int (length es) <+> hsep (map ppExpr es)
ppExpr (ERef n)   = char '?' <+> int n
