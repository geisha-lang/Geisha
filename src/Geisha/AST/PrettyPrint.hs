module Geisha.AST.PrettyPrint (
  showExpr,
  showType,
  showScheme
) where
import Data.List
import Text.PrettyPrint as PP

import Geisha.AST

showExpr = PP.render . prExpr
showType = PP.render . prType
showScheme = PP.render . prScheme
showLit = PP.render . prLit

instance Show GType where
  show = showType

instance Show Expr where
  show = showExpr
instance Show Lit where
  show = showLit
instance Show Scheme where
  -- show (Forall names ty) = unwords $ quantifier ++ [show ty]
  --   where quantifier = if null names then [] else ["∀", intercalate ", " names, "."]
  show = showScheme

instance Show Syntax where
  -- show (Expr (Annotation _ ty) exp)  = show exp ++ ":" ++ show ty
  -- show (Decl (Annotation _ ty) decl) = show decl ++ ":" ++ show ty
  show = PP.render . prSyntax
instance Show Lambda where
  show (Lambda p b) = "(" ++ intercalate ", " (map show p) ++ ") -> " ++ show b

instance Show Decl where
  show = PP.render . prDeclare
-- instance Show Syntax where
--   show (Expr (Annotation _ ty) exp)  = show exp ++ ":" ++ show ty
--   show (Decl (Annotation _ ty) decl) = show decl ++ ":" ++ show ty

prSyntax :: Syntax -> PP.Doc
prSyntax (Expr (Annotation _ ty) exp)  = prExpr exp <+> PP.text ":" <+> prScheme ty
prSyntax (Decl (Annotation _ ty) (Define n syn)) = PP.text "def" <+> PP.text n <+> PP.text ":" <+> prScheme ty <+> PP.text "=" $$ PP.nest 2 (prSyntax syn)

prDeclare (Define n syn) = PP.text "def" <+> PP.text n <+> PP.text "=" $$ PP.nest 2 (prSyntax syn)

prType             ::  GType -> PP.Doc
prType (TVar _ n)    =   PP.text n
prType (TCon _ c)    =   PP.text c
prType (TArr _ t s)  =   prType t <+> PP.text "⇒" <+> prType s
prType (TProd _ t s) =   prParenType t <+> PP.text "×" <+> prParenType s
prType Void{}       =   PP.text "∅"
prType TSlot{}       =   PP.text "_"

prParenType     ::  GType -> PP.Doc
prParenType  t@TArr{} = PP.parens (prType t)
prParenType t           = prType t

prExpr                  ::  Expr -> PP.Doc
prExpr (Var name)      =   PP.text name
prExpr (Lit lit)       =   prLit lit
prExpr (Let x b body)  =   PP.text "let" <+> 
                           prSyntax x <+> PP.text "=" <+>
                           prSyntax b <+> PP.text "in" PP.$$
                           PP.nest 2 (prSyntax body)
prExpr (Apply e1 e2)     =   prSyntax e1 <> PP.parens (PP.hsep . intersperse comma $ map prSyntax e2)
prExpr (List es) = PP.brackets (PP.hsep . intersperse comma $ map prSyntax es)
prExpr (Block es) = PP.braces (PP.vcat $ map prSyntax es)
prExpr (If cond tr tf) = PP.text "if" <+> PP.parens (prSyntax cond)
                          PP.$$ PP.nest 2 (prSyntax tr)
                          PP.$$ PP.text "else"
                          PP.$$ PP.nest 2 (prSyntax tf)
prExpr (Function (Lambda n e))       =   (PP.parens . PP.hsep . intersperse comma $ map prSyntax n) <+>
                           PP.text "->" <+>
                           prSyntax e

                                                                   
prParenExp    ::  Expr -> PP.Doc
prParenExp t  =   case t of
                    Let{}      -> PP.parens (prExpr t)
                    Apply{}    -> PP.parens (prExpr t)
                    Function{} -> PP.parens (prExpr t)
                    _          -> prExpr t

prLit            ::  Lit -> PP.Doc
prLit (LInt i)   =   PP.integer i
prLit (LBool b)  =   if b then PP.text "True" else PP.text "False"
prLit (LFloat f) =   PP.double f
prLit (LStr b)   =   PP.doubleQuotes $ PP.text b

prScheme                  ::  Scheme -> PP.Doc
prScheme (Forall vars t)  =   PP.text "∀" <+>
                              PP.hcat 
                                (PP.punctuate PP.comma (map PP.text vars))
                              PP.<+> PP.text "." <+> prType t
