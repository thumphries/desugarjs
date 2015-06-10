module Desugar.Currying (go) where

import Data.Data
import Data.Foldable
import Data.Generics.Aliases
import Data.Generics.Schemes
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax

go :: Data a => a -> a
go = everywhere (mkT desugar)

-- XXX Desugar top-level function decls
-- XXX Oh god guarded decls, what do?

desugar :: Exp -> Exp
desugar e@(Lambda loc pats body) =
  case pats of (_:_:_) -> (unlam pats) body
               _       -> e
desugar e = e

unlam :: [Pat] -> Exp -> Exp
unlam (x:xs) e = Lambda noloc [x] (unlam xs e)
unlam []     e = e

-- XXX This is probably bad?
noloc :: SrcLoc
noloc = SrcLoc "" 0 0
