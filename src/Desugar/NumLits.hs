module Desugar.NumLits (go) where

import Data.Data
import Data.Foldable
import Data.Generics.Aliases
import Data.Generics.Schemes
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax


go :: Data a => a -> a
go = everywhere (mkT desugar)

desugar :: Exp -> Exp
desugar e@(Lit (Int _))  = fromIntLit e
desugar e@(Lit (Frac _)) = fromRatLit e
desugar e = e

fromIntLit :: Exp -> Exp
fromIntLit e = App (Var (UnQual (Ident "fromIntegral")))
                   (Paren (ExpTypeSig noloc e
                          (TyCon (UnQual (Ident "Integer")))))

fromRatLit :: Exp -> Exp
fromRatLit e = App (Var (UnQual (Ident "fromRational")))
                   (Paren (ExpTypeSig noloc e
                          (TyCon (UnQual (Ident "Rational")))))

-- XXX This is probably bad?
noloc :: SrcLoc
noloc = SrcLoc "" 0 0
