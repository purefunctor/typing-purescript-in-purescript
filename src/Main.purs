module Main where

import Prelude

import Data.Array as Array
import Data.Maybe (fromMaybe)
import Data.Tuple as Tuple
import Effect (Effect)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Types (Ident, IntValue, Label, Name(..), Proper, QualifiedName, RecordLabeled, Separated(..), SourceRange, Wrapped(..))
import PureScript.CST.Types as CST
import Unsafe.Coerce (unsafeCoerce)

sourceFile :: String
sourceFile =
  """
module Main where
"""

type Annotation =
  { range :: SourceRange
  }

data ExprKind
  -- Uppercase identifier e.g. Just
  = ExprConstructor (QualifiedName Proper)
  -- Lowercase identifier e.g. foo
  | ExprVariable (QualifiedName Ident)
  -- See `LiteralKind`
  | ExprLiteral LiteralKind
  -- See `TernaryFields`
  | ExprTernary TernaryFields
  -- A typed hole e.g. ?hello
  | ExprHole (Name Ident)

data LiteralKind
  -- `true` or `false`
  = BooleanLiteral Boolean
  -- 'a'
  | CharLiteral Char
  -- "hello!"
  | StringLiteral String
  -- 42
  | IntLiteral IntValue
  -- 42.0
  | NumberLiteral Number
  -- ["hello!", 42, 42.0, 'a']
  | ArrayLiteral (Array Expr)
  -- { a, b: "hello!" }
  | RecordLiteral (Array RecordFieldKind)

data RecordFieldKind
  -- { a }
  = RecordPun Ident
  -- { a: 42 }
  | RecordField Label Expr

type TernaryFields =
  { if :: Expr
  , then :: Expr
  , else :: Expr
  }

newtype Expr = Expr
  { annotation :: Annotation
  , exprKind :: ExprKind
  }

convertExpr :: CST.Expr Void -> Expr
convertExpr e = Expr { annotation, exprKind }
  where
  annotation :: Annotation
  annotation = { range: rangeOf e }

  exprKind :: ExprKind
  exprKind = case e of
    CST.ExprHole n -> ExprHole n
    CST.ExprConstructor c -> ExprConstructor c
    CST.ExprIdent i -> ExprVariable i
    CST.ExprBoolean _ b -> ExprLiteral $ BooleanLiteral b
    CST.ExprChar _ c -> ExprLiteral $ CharLiteral c
    CST.ExprString _ s -> ExprLiteral $ StringLiteral s
    CST.ExprInt _ i -> ExprLiteral $ IntLiteral i
    CST.ExprNumber _ n -> ExprLiteral $ NumberLiteral n
    CST.ExprIf i -> ExprTernary
      { if: convertExpr i.cond
      , then: convertExpr i.true
      , else: convertExpr i.false
      }
    CST.ExprArray (Wrapped { value }) -> do
      let
        convertArray :: Separated (CST.Expr Void) -> Array Expr
        convertArray (Separated { head, tail }) =
          Array.cons (convertExpr head) (convertExpr <<< Tuple.snd <$> tail)
      ExprLiteral $ ArrayLiteral $ fromMaybe [] $ convertArray <$> value
    CST.ExprRecord (Wrapped { value }) -> do
      let
        convertField :: RecordLabeled (CST.Expr Void) -> RecordFieldKind
        convertField = case _ of
          CST.RecordPun (Name { name }) -> RecordPun name
          CST.RecordField (Name { name }) _ v -> RecordField name $ convertExpr v

        convertRecord :: Separated (RecordLabeled (CST.Expr Void)) -> Array RecordFieldKind
        convertRecord (Separated { head, tail }) =
          Array.cons (convertField head) (convertField <<< Tuple.snd <$> tail)
      ExprLiteral $ RecordLiteral $ fromMaybe [] $ convertRecord <$> value
    _ -> unsafeCoerce "Unimplemented!"

{-

# Challenges

1. `ExprSection` is a token that denotes `_`, which is shorthand syntax for defining functions.
    For example, the expression `if _ then _ else _` should expand to something like:

      \v0 v1 v2 -> if v0 then v1 else v2

    We can either:
    1. Transform the CST such that cases like `ExprSection` is desugared
    2. Implement desugaring by defining separate core and "surface" types

# Decisions

1. For the purposes of this project, which is implementing type checking PureScript in PureScript,
   I feel that that the option of desugaring the CST directly rather than implementing desugaring
   through separate "surface" and "core" types achieves the goal much faster. That being said,
   there's a few more cases of desugaring that might change this decision.

-}

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
