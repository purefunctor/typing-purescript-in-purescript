-- | The "surface" syntax for the language.
-- | 
-- | This serves as an intermediary between the CST and Core types and it's used to implement
-- | certain syntactic desugaring passes such as converting "sections" to lambdas and matching
-- | top-level type declarations and value declarations.
module Language.PureScript.Surface where

import Prelude
import Prim hiding (Type)

import Control.Monad.State (State, execState, gets, modify_)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (bimap)
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Language.PureScript.Annotation (Annotation)
import Language.PureScript.Constants (primFunction)
import Language.PureScript.Constants as Constants
import Language.PureScript.Names (QualifiedName, convertQualifiedName)
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Types (Ident, IntValue, Label, Labeled(..), Name(..), Operator, Proper, RecordLabeled, Row(..), Separated(..), Wrapped(..))
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
  | ExprApp (AppFields Expr)
  -- A lambda expression e.g. \x -> x
  | ExprLambda LambdaFields
  -- A case expression
  | ExprCase CaseFields
  -- A let expression
  | ExprLet LetFields
  -- An annotated expression
  | ExprTyped Expr Type
  -- A do block
  | ExprDo (NonEmptyArray DoStatement)
  -- An ado block
  | ExprAdo AdoFields
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

type AppFields e =
  { head :: e
  , spine :: NonEmptyArray e
  }

type LambdaFields =
  { binders :: NonEmptyArray Binder
  , expression :: Expr
  }

type CaseFields =
  { expressions :: NonEmptyArray Expr
  , branches :: NonEmptyArray (Tuple (NonEmptyArray Binder) Guarded)
  }

type LetFields =
  { bindings :: NonEmptyArray LetBinding
  , expression :: Expr
  }

data LetBinding
  -- A value binding
  = LetBindingName ValueBindingFields
  -- A pattern
  | LetBindingPattern Binder Where

type ValueBindingFields =
  { name :: Ident
  , binders :: Array Binder
  , guarded :: Guarded
  , maybeType :: Maybe Unit
  }

newtype Where = Where
  { expression :: Expr
  , bindings :: Maybe (NonEmptyArray LetBinding)
  }

data Guarded
  -- Always true, no guard
  = Unconditional Where
  -- A sequence of guards
  | Guarded (NonEmptyArray GuardedExpr)

newtype GuardedExpr = GuardedExpr
  { patterns :: NonEmptyArray PatternGuard
  , where :: Where
  }

newtype PatternGuard = PatternGuard
  { binder :: Maybe Binder
  , expression :: Expr
  }

data DoStatement
  = DoLet (NonEmptyArray LetBinding)
  | DoDiscard Expr
  | DoBind Binder Expr

type AdoFields =
  { statements :: Array DoStatement
  , result :: Expr
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
  -- r { foo = value }
  = RecordUpdateLeaf Label Expr
  -- r { foo { bar = value } }
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
    CST.ExprTyped v _ t -> ExprTyped (convertExpr v) (convertType t)
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
      { head: Expr
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
    CST.ExprApp head spine -> ExprApp
      { head: convertExpr head, spine: convertExpr <$> spine }
    CST.ExprLambda { binders, body } -> ExprLambda
      { binders: convertBinder <$> binders, expression: convertExpr body }
    CST.ExprCase { head: Separated { head, tail }, branches } -> do
      let
        convertBranch
          :: Tuple (Separated (CST.Binder Void)) (CST.Guarded Void)
          -> Tuple (NonEmptyArray Binder) Guarded
        convertBranch (Tuple (Separated { head: binderHead, tail: binderTail }) guarded) =
          Tuple
            ( NonEmptyArray.cons' (convertBinder binderHead)
                (Tuple.snd >>> convertBinder <$> binderTail)
            )
            (convertGuarded guarded)
      ExprCase
        { expressions: NonEmptyArray.cons' (convertExpr head) (Tuple.snd >>> convertExpr <$> tail)
        , branches: convertBranch <$> branches
        }
    CST.ExprLet { bindings, body } ->
      ExprLet
        { bindings: convertLetBindings bindings
        , expression: convertExpr body
        }
    CST.ExprDo { statements } ->
      ExprDo $ convertDoStatement <$> statements
    CST.ExprAdo { statements, result } ->
      ExprAdo { statements: convertDoStatement <$> statements, result: convertExpr result }
    CST.ExprError v -> absurd v

  convertLetBindings :: NonEmptyArray (CST.LetBinding Void) -> NonEmptyArray LetBinding
  convertLetBindings bindings = do
    let
      convertLetBinding :: CST.LetBinding Void -> State _ _
      convertLetBinding = case _ of
        CST.LetBindingSignature (Labeled { label: Name { name } }) ->
          modify_ $ \s -> s { signatures = M.insert name unit s.signatures }
        CST.LetBindingName { name: Name { name }, binders, guarded } -> do
          maybeType <- gets (\s -> M.lookup name s.signatures)
          modify_ $ \s -> s
            { bindings = Array.snoc s.bindings $ LetBindingName
                { name
                , binders: convertBinder <$> binders
                , guarded: convertGuarded guarded
                , maybeType
                }
            }
        CST.LetBindingPattern b _ w ->
          modify_ $ \s -> s
            { bindings = Array.snoc s.bindings
                $ LetBindingPattern (convertBinder b) (convertWhere w)
            }
        CST.LetBindingError v -> absurd v

      result :: { signatures :: Map Ident Unit, bindings :: Array LetBinding }
      result = execState (traverse_ convertLetBinding $ NonEmptyArray.toArray bindings)
        { signatures: M.empty, bindings: [] }

    case NonEmptyArray.fromArray result.bindings of
      Just finalBindings | M.isEmpty result.signatures ->
        finalBindings
      _ | not $ M.isEmpty result.signatures ->
        unsafeCrashWith "Invariant violated: at least one LetBindingSignature remains"
      _ ->
        unsafeCrashWith
          "Invariant violated: at least one LetBindingName or LetBindingPattern should occur"

  convertWhere :: CST.Where Void -> Where
  convertWhere (CST.Where { expr, bindings }) = Where
    { expression: convertExpr expr
    , bindings: Tuple.snd >>> convertLetBindings <$> bindings
    }

  convertGuarded :: CST.Guarded Void -> Guarded
  convertGuarded = case _ of
    CST.Unconditional _ w -> Unconditional $ convertWhere w
    CST.Guarded g -> Guarded $ convertGuardedExpr <$> g

  convertGuardedExpr :: CST.GuardedExpr Void -> GuardedExpr
  convertGuardedExpr (CST.GuardedExpr { patterns: Separated { head, tail }, "where": guardedWhere }) =
    GuardedExpr
      { patterns: NonEmptyArray.cons' (convertPatternGuard head)
          (Tuple.snd >>> convertPatternGuard <$> tail)
      , "where": convertWhere guardedWhere
      }

  convertPatternGuard :: CST.PatternGuard Void -> PatternGuard
  convertPatternGuard (CST.PatternGuard { binder, expr }) = PatternGuard
    { binder: Tuple.fst >>> convertBinder <$> binder, expression: convertExpr expr }

  convertDoStatement :: CST.DoStatement Void -> DoStatement
  convertDoStatement = case _ of
    CST.DoLet _ l -> DoLet (convertLetBindings l)
    CST.DoDiscard s -> DoDiscard (convertExpr s)
    CST.DoBind b _ s -> DoBind (convertBinder b) (convertExpr s)
    CST.DoError v -> absurd v

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
  | BinderTyped Binder Type
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

        convertRecord
          :: Separated (RecordLabeled (CST.Binder Void)) -> Array (RecordFieldKind Binder)
        convertRecord (Separated { head, tail }) =
          Array.cons (convertField head) (convertField <<< Tuple.snd <$> tail)
      BinderLiteral $ RecordLiteral $ fromMaybe [] $ convertRecord <$> value
    CST.BinderParens (Wrapped { value }) -> BinderParens $ convertBinder value
    CST.BinderTyped v _ t -> BinderTyped (convertBinder v) (convertType t)
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
    CST.BinderError v -> absurd v

data TypeKind
  -- A type variable
  = TypeVar Ident
  -- A type constructor
  | TypeConstructor (QualifiedName Proper)
  -- An operator identifier
  | TypeOperator (QualifiedName Operator)
  -- A type wildcard
  | TypeWildcard
  -- A typed hole
  | TypeHole Ident
  -- A type-level string
  | TypeString String
  -- A type-level int
  | TypeInt IntValue
  -- A row type
  | TypeRow RowFields
  -- A record type
  | TypeRecord RowFields
  -- A universally quantified type
  | TypeForall (NonEmptyArray TypeVarBinding) Type
  -- A kind-annotated type
  | TypeKinded Type Type
  -- A type application
  | TypeApp (AppFields Type)
  -- A chain of binary type operations
  | TypeOp (OpFields Type)
  -- A function type e.g. a -> b
  | TypeArrow Type Type
  -- A type constrained by a type class
  | TypeConstrained (NonEmptyArray (Tuple (QualifiedName Proper) (Array Type))) Type
  -- A parenthesized type
  | TypeParens Type

type RowFields =
  { labels :: Maybe (NonEmptyArray (Tuple Label Type))
  , tail :: Maybe Type
  }

data TypeVarBinding
  -- A kind-annotated type variable
  = TypeVarKinded Ident Type
  -- A type variable
  | TypeVarName Ident

newtype Type = Type
  { annotation :: Annotation
  , kind :: TypeKind
  }

convertType :: CST.Type Void -> Type
convertType ty = Type { annotation, kind }
  where
  annotation :: Annotation
  annotation = { range: rangeOf ty }

  kind :: TypeKind
  kind = case ty of
    CST.TypeVar (Name { name }) -> TypeVar name
    CST.TypeConstructor n -> TypeConstructor $ convertQualifiedName n
    CST.TypeOpName n -> TypeOperator $ convertQualifiedName n
    CST.TypeWildcard _ -> TypeWildcard
    CST.TypeHole (Name { name }) -> TypeHole name
    CST.TypeString _ v -> TypeString v
    CST.TypeInt _ _ v -> TypeInt v
    CST.TypeRow (Wrapped { value: Row { labels, tail } }) ->
      TypeRow { labels: convertLabels <$> labels, tail: Tuple.snd >>> convertType <$> tail }
    CST.TypeRecord (Wrapped { value: Row { labels, tail } }) ->
      TypeRecord { labels: convertLabels <$> labels, tail: Tuple.snd >>> convertType <$> tail }
    CST.TypeForall _ b _ t -> do
      let
        convertTypeVarBinding :: CST.TypeVarBinding Void -> TypeVarBinding
        convertTypeVarBinding = case _ of
          CST.TypeVarKinded (Wrapped { value: Labeled { label: Name { name }, value } }) ->
            TypeVarKinded name (convertType value)
          CST.TypeVarName (Name { name }) ->
            TypeVarName name
      TypeForall (convertTypeVarBinding <$> b) (convertType t)
    CST.TypeKinded t _ k ->
      TypeKinded (convertType t) (convertType k)
    CST.TypeApp head spine ->
      TypeApp { head: convertType head, spine: convertType <$> spine }
    CST.TypeOp head tail -> do
      let
        convertTail
          :: Tuple (CST.QualifiedName Operator) (CST.Type Void)
          -> Tuple (QualifiedName Operator) Type
        convertTail = bimap convertQualifiedName convertType
      TypeOp { head: convertType head, tail: convertTail <$> tail }
    CST.TypeArrow t _ u ->
      TypeArrow (convertType t) (convertType u)
    CST.TypeArrowName _ ->
      TypeConstructor primFunction
    CST.TypeConstrained c _ t -> do
      let
        parseClass :: CST.Type Void -> Tuple (QualifiedName Proper) (Array Type)
        parseClass = case _ of
          CST.TypeConstructor name -> 
            Tuple (convertQualifiedName name) []
          CST.TypeApp (CST.TypeConstructor name) spine -> 
            Tuple (convertQualifiedName name) (convertType <$> NonEmptyArray.toArray spine)
          _ ->
            unsafeCrashWith "Invariant violated: failed to parse constraint"

        parseClassUntil :: Array _ -> CST.Type Void -> TypeKind
        parseClassUntil accumulator = case _ of
          CST.TypeConstrained constraint _ deeper -> 
            parseClassUntil (Array.snoc accumulator $ parseClass constraint) deeper
          deeper ->
            TypeConstrained (NonEmptyArray.cons' (parseClass c) accumulator) $ convertType deeper

      parseClassUntil [] t
    CST.TypeParens (Wrapped { value }) ->
      TypeParens $ convertType value
    CST.TypeError v -> absurd v

  convertLabel :: Labeled (Name Label) (CST.Type Void) -> Tuple Label Type
  convertLabel (Labeled { label: Name { name }, value }) = Tuple name (convertType value)

  convertLabels
    :: Separated (Labeled (Name Label) (CST.Type Void)) -> NonEmptyArray (Tuple Label Type)
  convertLabels (Separated { head, tail }) = NonEmptyArray.cons' (convertLabel head)
    (Tuple.snd >>> convertLabel <$> tail)
