{-# LANGUAGE RecordWildCards #-}

module Language.Slugs.PP where

import Language.Slugs.AST

import Text.PrettyPrint.HughesPJ (Doc,vcat,text,int,hsep,char,(<+>),(<>))


ppSpec :: Spec -> Doc
ppSpec Spec { .. } =
  vcat [ text "INPUT"
       , vcat (map ppDecl specInput)

       , text "OUTPUT"
       , vcat (map ppDecl specOutput)

       , text "ENV_INIT"
       , ppTopExpr (stInit specEnv)

       , text "ENV_TRANS"
       , ppTopExpr (stTrans specEnv)

       , text "ENV_LIVELINESS"
       , ppTopExpr (stLiveness specEnv)

       , text "SYS_INIT"
       , ppTopExpr (stInit specSys)

       , text "SYS_TRANS"
       , ppTopExpr (stTrans specSys)

       , text "SYS_LIVELINESS"
       , ppTopExpr (stLiveness specSys)
       ]


ppDecl :: Decl -> Doc
ppDecl (DeclVar v)     = text v
ppDecl (DeclNum v l h) =
  let var = VarNum v l h
   in vcat (ppBit0 var : [ ppBitN var i | i <- [ 1 .. varBitSize var - 1 ] ])

ppBit0 :: Var -> Doc
ppBit0 v@(VarNum _ l h) = ppBitN v 0 <> char '.' <> int l <> char '.' <> int h
ppBit0 VarBool{}        = error "ppBit0: expected a VarNum"

ppBitN :: Var -> Int -> Doc
ppBitN (VarNum v _ _) i = text v <> char '@' <> int i
ppBitN VarBool{}      _ = error "ppBitN: expected a VarNum"


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
ppExpr (EBit v 0) = ppBit0 v
ppExpr (EBit v i) = ppBitN v i
ppExpr ETrue      = char '1'
ppExpr EFalse     = char '0'
ppExpr (EBuf es)  = char '$' <+> int (length es) <+> hsep (map ppExpr es)
ppExpr (ERef n)   = char '?' <+> int n
