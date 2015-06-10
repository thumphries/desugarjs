
module Main where

import Data.Data
import Data.Default
import Data.Foldable
import Data.Generics.Aliases
import Data.Generics.Schemes
import qualified Data.Map as Map
import Language.Haskell.HsColour.HTML (hscolour)
import Language.Haskell.HsColour.Colourise (defaultColourPrefs)
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax

import Reflex
import Reflex.Dom

import Desugar.DoNotation as Do
import Desugar.IfThenElse as If
import Desugar.ListComp   as ListComp

main = mainWidget $ do
  el "h1" (text "Haskell Desugarer")
  el "p"  (text "Paste valid Haskell here and see what happens.")
  el "p" (text "This was built hastily with Reflex, GHCJS, \
               \ haskell-src-exts, and hscolour. \
               \ Doesn't support most GHC extensions, for now.")
  el "div" $ do
    menu      <- dropdown "unrolldo" (constDyn transformations) def
    input     <- codeInput
    result    <- combineDyn (\t c -> strToFun t <$> c)
                            (_dropdown_value menu) input
    resultDbl <- mapDyn pretty result
    displayWidget resultDbl
    return ()

pretty :: ParseResult Module -> String
pretty (ParseOk mod) = colourise (prettyPrint mod)
pretty err = show err

colourise = hscolour defaultColourPrefs False 0

codeInput :: MonadWidget t m => m (Dynamic t (ParseResult Module))
codeInput = do
  let cfg = TextAreaConfig defaultCode never (constDyn attrs)
      attrs = Map.singleton "class" "input"
  ta <- textArea cfg
  result <- mapDyn parseModule (_textArea_value ta)
  return result

displayWidget :: MonadWidget t m => Dynamic t String -> m (El t)
displayWidget = elDynHtmlAttr' "div" (Map.singleton "class" "output")

transformations :: Map.Map String String
transformations = Map.fromList
  [("unrolldo",   "Desugar do-notation")
  ,("unrollif",   "Desugar if-then-else")
  ,("listcompdo", "Desugar list comprehensions")
  ,("id",         "Do nothing")
  ]

strToFun :: Data a => String -> a -> a
strToFun s = case s of
  "unrolldo"   -> Do.go
  "unrollif"   -> If.go
  "listcompdo" -> ListComp.go
  "id"         -> id
  _            -> error ("Bad input: " ++ s)

-- XXX Would be fun to TH-splice this file in
defaultCode :: String
defaultCode = "main :: IO ()\nmain = do\n  a <- b\n  return (do return a)"

