module Language.PureScript.Annotation where

import PureScript.CST.Types (SourceRange)

type Annotation =
  { range :: SourceRange
  }
