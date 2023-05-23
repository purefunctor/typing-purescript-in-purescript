-- | The "surface" syntax for the language.
-- | 
-- | This serves as an intermediary between the CST and Core types and it's used to implement
-- | certain syntactic desugaring passes such as converting "sections" to lambdas and matching
-- | top-level type declarations and value declarations.
module Language.PureScript.Surface where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple as Tuple
import Language.PureScript.Annotation (Annotation)
import Language.PureScript.Constants as Constants
import Language.PureScript.Names (QualifiedName, convertQualifiedName)
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Types (Ident, IntValue, Label, Name(..), Operator, Proper, RecordLabeled, Separated(..), Wrapped(..))
import PureScript.CST.Types as CST

data ExprKind
  -- Uppercase identifier e.g. Just
  = ExprConstructor (QualifiedName Proper)
  -- Lowercase identifier e.g. foo
  | ExprVariable (QualifiedName Ident)
  -- Operator identifier e.g. (+)
  | ExprOperator (QualifiedName Operator)
  -- A literal value
  | ExprLiteral LiteralKind
  -- A ternary expression
  | ExprTernary TernaryFields
  -- A function application e.g. f a b
  | ExprApp AppFields
  -- A record access e.g. x.foo.bar
  | ExprRecordAccessor RecordAccessorFields
  -- A typed hole e.g. ?hello
  | ExprHole Ident
  -- Marks a section expression
  | ExprSection
  -- A parenthesized expression
  | ExprParens Expr

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

type AppFields =
  { function :: Expr
  , spine :: NonEmptyArray Expr
  }

type RecordAccessorFields =
  { record :: Expr
  , path :: NonEmptyArray Label
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
    CST.ExprHole (Name { name }) -> ExprHole name
    CST.ExprConstructor c -> ExprConstructor $ convertQualifiedName c
    CST.ExprIdent i -> ExprVariable $ convertQualifiedName i
    CST.ExprOpName o -> ExprOperator $ convertQualifiedName o
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
    CST.ExprSection _ -> ExprSection
    CST.ExprParens (Wrapped { value }) -> ExprParens $ convertExpr value
    CST.ExprTyped _ _ _ -> unsafeCrashWith "Unimplemented!"
    CST.ExprInfix _ _ -> unsafeCrashWith "Unimplemented!"
    CST.ExprOp _ _ -> unsafeCrashWith "Unimplemented!"
    CST.ExprNegate n v -> ExprApp
      { function: Expr
          { annotation: { range: n.range }
          , exprKind: ExprVariable Constants.preludeNegate
          }
      , spine: NonEmptyArray.cons' (convertExpr v) []
      }
    CST.ExprRecordAccessor { expr, path: Separated { head: Name { name }, tail } } -> do
      let convertTail = map (Tuple.snd >>> unwrap >>> _.name)
      ExprRecordAccessor
        { record: convertExpr expr
        , path: NonEmptyArray.cons' name $ convertTail tail
        }
    CST.ExprRecordUpdate _ _ -> unsafeCrashWith "Unimplemented!"
    CST.ExprApp function spine -> ExprApp
      { function: convertExpr function, spine: convertExpr <$> spine }
    CST.ExprLambda _ -> unsafeCrashWith "Unimplemented!"
    CST.ExprCase _ -> unsafeCrashWith "Unimplemented!"
    CST.ExprLet _ -> unsafeCrashWith "Unimplemented!"
    CST.ExprDo _ -> unsafeCrashWith "Unimplemented!"
    CST.ExprAdo _ -> unsafeCrashWith "Unimplemented!"
    CST.ExprError v -> absurd v
