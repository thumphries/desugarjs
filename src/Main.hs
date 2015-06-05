
module Main where

import Data.Data
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

main = mainWidget $ do
  el "p" (text "Paste valid Haskell here and see what happens.")
  el "div" $ do
    input     <- codeInput
    resultDbl <- mapDyn pretty input
    displayWidget resultDbl
    return ()

pretty :: ParseResult Module -> String
pretty (ParseOk mod) = colourise (prettyPrint (Do.go mod))
pretty err = show err

colourise = hscolour defaultColourPrefs False 0

codeInput :: MonadWidget t m => m (Dynamic t (ParseResult Module))
codeInput = do
  let cfg = TextAreaConfig "main = undefined" never (constDyn attrs)
      attrs = Map.singleton "style" "width: 45vw; height: 90vh; float: left;"
  ta <- textArea cfg
  result <- mapDyn parseModule (_textArea_value ta)
  return result

displayWidget :: MonadWidget t m => Dynamic t String -> m (El t)
displayWidget = elDynHtmlAttr' "div" (Map.singleton "style"
                  "float: right; width: 50vw; height: 90vh;")
