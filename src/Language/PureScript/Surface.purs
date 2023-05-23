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
import Data.Bifunctor (bimap)
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple)
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
  | ExprLiteral (LiteralKind Expr)
  -- A ternary expression
  | ExprTernary TernaryFields
  -- A function application e.g. f a b
  | ExprApp AppFields
  -- A record access e.g. x.foo.bar
  | ExprRecordAccessor RecordAccessorFields
  -- A record update e.g. foo { bar = 1 }, foo { bar { baz = 0 } }
  | ExprRecordUpdate RecordUpdateFields
  -- A chain of binary operations e.g. 1 + 2 * 3
  | ExprOp (OpFields Expr)
  -- A chain of infix applications e.g. a `foo` b `foo` c
  | ExprInfix InfixFields
  -- A typed hole e.g. ?hello
  | ExprHole Ident
  -- Marks a section expression
  | ExprSection
  -- A parenthesized expression
  | ExprParens Expr

data LiteralKind e
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
  | ArrayLiteral (Array e)
  -- { a, b: "hello!" }
  | RecordLiteral (Array (RecordFieldKind e))

data RecordFieldKind e
  -- { a }
  = RecordPun Ident
  -- { a: 42 }
  | RecordField Label e

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

type RecordUpdateFields =
  { record :: Expr
  , update :: RecordUpdate
  }

data RecordUpdate
  = RecordUpdateLeaf Label Expr
  | RecordUpdateBranch Label RecordUpdate

type OpFields e =
  { head :: e
  , tail :: NonEmptyArray (Tuple (QualifiedName Operator) e)
  }

type InfixFields =
  { head :: Expr
  , tail :: NonEmptyArray (Tuple Expr Expr)
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
        convertField :: RecordLabeled (CST.Expr Void) -> RecordFieldKind Expr
        convertField = case _ of
          CST.RecordPun (Name { name }) -> RecordPun name
          CST.RecordField (Name { name }) _ v -> RecordField name $ convertExpr v

        convertRecord :: Separated (RecordLabeled (CST.Expr Void)) -> Array (RecordFieldKind Expr)
        convertRecord (Separated { head, tail }) =
          Array.cons (convertField head) (convertField <<< Tuple.snd <$> tail)
      ExprLiteral $ RecordLiteral $ fromMaybe [] $ convertRecord <$> value
    CST.ExprSection _ -> ExprSection
    CST.ExprParens (Wrapped { value }) -> ExprParens $ convertExpr value
    CST.ExprTyped _ _ _ -> unsafeCrashWith "Unimplemented!"
    CST.ExprInfix head tail -> do
      let
        convertTail :: Tuple (Wrapped (CST.Expr Void)) (CST.Expr Void) -> Tuple Expr Expr
        convertTail = bimap (unwrap >>> _.value >>> convertExpr) convertExpr
      ExprInfix
        { head: convertExpr head
        , tail: convertTail <$> tail
        }
    CST.ExprOp head tail -> do
      let
        convertTail
          :: Tuple (CST.QualifiedName Operator) (CST.Expr Void)
          -> Tuple (QualifiedName Operator) Expr
        convertTail = bimap convertQualifiedName convertExpr
      ExprOp
        { head: convertExpr head
        , tail: convertTail <$> tail
        }
    CST.ExprNegate n v -> ExprApp
      { function: Expr
          { annotation: { range: n.range }
          , exprKind: ExprVariable Constants.preludeNegate
          }
      , spine: NonEmptyArray.cons' (convertExpr v) []
      }
    CST.ExprRecordAccessor { expr, path: Separated { head: Name { name }, tail } } -> do
      let
        convertTail :: Array (Tuple _ (Name Label)) -> Array Label
        convertTail = map (Tuple.snd >>> unwrap >>> _.name)
      ExprRecordAccessor
        { record: convertExpr expr
        , path: NonEmptyArray.cons' name $ convertTail tail
        }
    CST.ExprRecordUpdate expr update -> do
      let
        convertUpdate :: Wrapped (Separated (CST.RecordUpdate Void)) -> RecordUpdate
        convertUpdate (Wrapped { value: Separated { head } }) = case head of
          CST.RecordUpdateLeaf (Name { name }) _ value -> RecordUpdateLeaf name $ convertExpr value
          CST.RecordUpdateBranch (Name { name }) branch -> RecordUpdateBranch name $ convertUpdate
            branch
      ExprRecordUpdate
        { record: convertExpr expr
        , update: convertUpdate update
        }
    CST.ExprApp function spine -> ExprApp
      { function: convertExpr function, spine: convertExpr <$> spine }
    CST.ExprLambda _ -> unsafeCrashWith "Unimplemented!"
    CST.ExprCase _ -> unsafeCrashWith "Unimplemented!"
    CST.ExprLet _ -> unsafeCrashWith "Unimplemented!"
    CST.ExprDo _ -> unsafeCrashWith "Unimplemented!"
    CST.ExprAdo _ -> unsafeCrashWith "Unimplemented!"
    CST.ExprError v -> absurd v

data BinderKind
  -- Matches any pattern
  = BinderWildcard
  -- Matches and binds a pattern
  | BinderVar Ident
  -- Matches a constructor
  | BinderConstructor (QualifiedName Proper) (Array Binder)
  -- Matches and binds a pattern to a name
  | BinderNamed Ident Binder
  -- Matches a literal pattern
  | BinderLiteral (LiteralKind Binder)
  -- Matches a parenthesized pattern
  | BinderParens Binder
  -- Matches a typed pattern
  | BinderTyped Binder Void
  -- Matches a chain of binary operations
  | BinderOp (OpFields Binder)

newtype Binder = Binder
  { annotation :: Annotation
  , kind :: BinderKind
  }

convertBinder :: CST.Binder Void -> Binder
convertBinder b = Binder { annotation, kind }
  where
  annotation :: Annotation
  annotation = { range: rangeOf b }

  kind :: BinderKind
  kind = case b of
    CST.BinderWildcard _ -> BinderWildcard
    CST.BinderVar (Name { name }) -> BinderVar name
    CST.BinderNamed (Name { name }) _ binder -> BinderNamed name $ convertBinder binder
    CST.BinderConstructor name fields ->
      BinderConstructor (convertQualifiedName name) (convertBinder <$> fields)
    CST.BinderBoolean _ v -> BinderLiteral $ BooleanLiteral v
    CST.BinderChar _ c -> BinderLiteral $ CharLiteral c
    CST.BinderString _ s -> BinderLiteral $ StringLiteral s
    CST.BinderInt _ _ i -> BinderLiteral $ IntLiteral i
    CST.BinderNumber _ _ n -> BinderLiteral $ NumberLiteral n
    CST.BinderArray (Wrapped { value }) -> do
      let 
        convertArray :: Separated (CST.Binder Void) -> Array Binder
        convertArray (Separated { head, tail }) =
          Array.cons (convertBinder head) (convertBinder <<< Tuple.snd <$> tail)
      BinderLiteral $ ArrayLiteral $ fromMaybe [] $ convertArray <$> value
    CST.BinderRecord (Wrapped { value }) -> do
      let
        convertField :: RecordLabeled (CST.Binder Void) -> RecordFieldKind Binder
        convertField = case _ of
          CST.RecordPun (Name { name }) -> RecordPun name
          CST.RecordField (Name { name }) _ v -> RecordField name $ convertBinder v

        convertRecord :: Separated (RecordLabeled (CST.Binder Void)) -> Array (RecordFieldKind Binder)
        convertRecord (Separated { head, tail }) =
          Array.cons (convertField head) (convertField <<< Tuple.snd <$> tail)
      BinderLiteral $ RecordLiteral $ fromMaybe [] $ convertRecord <$> value
    CST.BinderParens (Wrapped { value }) -> BinderParens $ convertBinder value
    CST.BinderTyped _ _ _ -> unsafeCrashWith "Unimplemented!"
    CST.BinderOp head tail -> do
      let
        convertTail
          :: Tuple (CST.QualifiedName Operator) (CST.Binder Void)
          -> Tuple (QualifiedName Operator) Binder
        convertTail = bimap convertQualifiedName convertBinder
      BinderOp
        { head: convertBinder head
        , tail: convertTail <$> tail
        }
    _ -> unsafeCrashWith "Unimplemented!"
