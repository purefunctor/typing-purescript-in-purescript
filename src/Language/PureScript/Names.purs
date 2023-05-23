module Language.PureScript.Names where

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import PureScript.CST.Types (ModuleName)
import PureScript.CST.Types as CST

newtype QualifiedName a = QualifiedName
  { moduleName :: Maybe ModuleName
  , name :: a
  }

derive instance Newtype (QualifiedName a) _

convertQualifiedName :: forall a. CST.QualifiedName a -> QualifiedName a
convertQualifiedName (CST.QualifiedName qn) = QualifiedName
  { moduleName: qn."module", name: qn.name }
