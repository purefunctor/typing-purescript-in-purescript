module Language.PureScript.Constants where

import Prelude

import Data.Maybe (Maybe(..))
import Language.PureScript.Names (QualifiedName(..))
import PureScript.CST.Types (Ident(..), ModuleName(..), Proper(..))

preludeNegate :: QualifiedName Ident
preludeNegate = QualifiedName { moduleName: Just $ ModuleName "Prelude", name: Ident "negate" }

primFunction :: QualifiedName Proper
primFunction = QualifiedName { moduleName: Just $ ModuleName "Prim", name: Proper "Function" }
