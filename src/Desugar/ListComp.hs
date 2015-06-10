module Desugar.ListComp (go) where

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
desugar (ListComp exp pats) = unfoldQuals pats exp
desugar e = e

unfoldQuals :: [QualStmt] -> (Exp -> Exp)
unfoldQuals quals =
  \e -> Do ((map fun quals) ++
            [Qualifier (App (Var (UnQual (Ident "return"))) e)])
  where fun (QualStmt stmt) = stmt
        fun _ = error "-XTransformListComp not supported"

-- XXX This is probably bad?
noloc :: SrcLoc
noloc = SrcLoc "" 0 0
