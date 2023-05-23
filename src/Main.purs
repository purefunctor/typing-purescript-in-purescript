module Main where

import Prelude

import Effect (Effect)
import PureScript.CST (RecoveredParserResult(..), parseModule)

sourceFile :: String
sourceFile =
  """
module Main where
"""

example :: Effect Unit
example = case parseModule sourceFile of
  ParseSucceeded _ ->
    pure unit
  ParseSucceededWithErrors _ _ ->
    pure unit
  ParseFailed _ ->
    pure unit

main :: Effect Unit
main = pure unit
