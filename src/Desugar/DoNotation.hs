module Desugar.DoNotation (go) where

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
desugar (Do ls) = desugarDo ls
desugar e = e

desugarDo :: [Stmt] -> Exp
desugarDo xs = foldr' foldFun ret rest
  where (Qualifier ret) = last xs
        rest            = init xs

foldFun :: Stmt -> Exp -> Exp
foldFun (Generator loc var fun) expr =
  InfixApp fun bind (Lambda loc [var] expr)
foldFun (Qualifier e1) e2 =
  InfixApp e1 bind (Lambda noloc [PWildCard] e2)
foldFun (LetStmt bnds) e = Let bnds e
-- No idea how to desugar these arrow expressions.
foldFun (RecStmt _stmts) _e = error "-XArrows not supported."

bind :: QOp
bind = QVarOp (UnQual (Symbol ">>="))

-- XXX This is probably bad?
noloc :: SrcLoc
noloc = SrcLoc "" 0 0
