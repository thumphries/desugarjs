module Desugar.IfThenElse (go) where

import Data.Data
import Data.Foldable
import Data.Generics.Aliases
import Data.Generics.Schemes
import Language.Haskell.Exts.Syntax

go :: Data a => a -> a
go = everywhere (mkT desugar)

desugar :: Exp -> Exp
desugar (If p b1 b2) =
  Case p [Alt noloc (PApp (UnQual (Ident "True"))  [])
                    (UnGuardedRhs b1) (BDecls [])
         ,Alt noloc (PApp (UnQual (Ident "False")) [])
                    (UnGuardedRhs b2) (BDecls [])
         ]
desugar e = e

-- XXX This is probably bad?
noloc :: SrcLoc
noloc = SrcLoc "" 0 0
