module Deku.Vike.Path where

import Prelude

import Data.Either (Either(..))
import Data.List ((:), List(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple)
import Deku.Vike.Spec (VikeSpec)
import Effect.Aff (Aff)
import Parsing (ParseError(..), Position(..))
import Prim.Row as Rw
import Record as Rec
import TLDR.Combinators as C
import TLDR.Combinators.Class (class ShowParser, SP1)
import TLDR.Combinators.Class as CC
import TLDR.List as L
import TLDR.Matchers as M
import TLDR.Result as R
import TLDR.Sugar (L1, WSM)
import Type.Proxy (Proxy(..))
import Yoga.JSON (class ReadForeign, class WriteForeign)

data PathSegment :: Type -> Type
data PathSegment a

instance ShowParser (SP1 "PathSegment" a) doc => ShowParser (PathSegment a) doc

data LiteralPath :: Type -> Type
data LiteralPath a

instance ShowParser (SP1 "LiteralPath" a) doc => ShowParser (LiteralPath a) doc

data WildcardPath :: Type -> Type
data WildcardPath a

instance
  ShowParser (SP1 "WildcardPath" a) doc =>
  ShowParser (WildcardPath a) doc

type ParseLiteralPath = LiteralPath (M.Some (M.Except (M.Literal "/") M.Any))

type ParseWildcard = C.IgnoreAndThenParse
  (L1 ":")
  (WildcardPath (M.Some (M.Except (M.Literal "/") M.Any)))

type ParsePathSegment = C.Or ParseWildcard ParseLiteralPath

type ParsePath = C.IgnoreAndThenParse
  (L1 "/")
  (PathSegment (C.SepBy ParsePathSegment (WSM (M.Literal "/"))))

testPath
  :: forall @toParse @h @t
   . CC.Parse toParse ParsePath Unit (R.Success h t) Unit
  => Unit
testPath = unit

testPath0 = testPath @"/" @(PathSegment L.Nil) @"" :: Unit
testPath1 =
  testPath @"/user/:id"
    @( PathSegment
        ( L.Cons (LiteralPath (Proxy "user"))
            (L.Cons (WildcardPath (Proxy "id")) L.Nil)
        )
    )
    @"" :: Unit

class PathSegmentsToDataFunctionSignature :: Type -> Type -> Constraint
class
  PathSegmentsToDataFunctionSignature pathSegment functionSignature
  | pathSegment -> functionSignature

instance
  ( WriteForeign a
  , ReadForeign a
  ) =>
  PathSegmentsToDataFunctionSignature L.Nil ({} -> Aff a)

instance
  PathSegmentsToDataFunctionSignature b c =>
  PathSegmentsToDataFunctionSignature (L.Cons (LiteralPath p) b) c

instance
  ( Rw.Lacks p r'
  , WriteForeign a
  , ReadForeign a
  , Rw.Cons p String r' r
  , PathSegmentsToDataFunctionSignature b ({ | r' } -> Aff a)
  ) =>
  PathSegmentsToDataFunctionSignature (L.Cons (WildcardPath (Proxy p)) b)
    ({ | r } -> Aff a)

testPathSegmentsToDataFunctionSignature
  :: forall @toParse @h @t @sig
   . CC.Parse toParse ParsePath Unit (R.Success (PathSegment h) t) Unit
  => PathSegmentsToDataFunctionSignature h sig
  => Unit
testPathSegmentsToDataFunctionSignature = unit

testPathSegmentsToDataFunctionSignature0 =
  testPathSegmentsToDataFunctionSignature @"/" @L.Nil @"" @({} -> Aff String)
    :: Unit

testPathSegmentsToDataFunctionSignature1 =
  testPathSegmentsToDataFunctionSignature @"/user/:id"
    @( L.Cons (LiteralPath (Proxy "user"))
        (L.Cons (WildcardPath (Proxy "id")) L.Nil)
    )
    @""
    @({ id :: String } -> Aff String) :: Unit

class PathToVike :: Symbol -> Type -> Constraint
class PathToVike path vike | path -> vike

instance
  ( CC.Parse path ParsePath Unit (R.Success (PathSegment seg) "") Unit
  , PathSegmentsToDataFunctionSignature seg ({ | r } -> Aff a)
  ) =>
  PathToVike path (Tuple ({ | r } -> Aff a) (VikeSpec a r))

testPathToVike :: forall @path @vike. PathToVike path vike => Unit
testPathToVike = unit

testPathToVike0 =
  testPathToVike @"/" @(Tuple ({} -> Aff String) (VikeSpec String ()))
    :: Unit

testPathToVike1 =
  testPathToVike @"/" @(Tuple ({} -> Aff Int) (VikeSpec Int ())) :: Unit

testPathToVike2 =
  testPathToVike @"/foo/bar" @(Tuple ({} -> Aff Int) (VikeSpec Int ()))
    :: Unit

testPathToVike3 =
  testPathToVike @"/foo/bar/:baz"
    @(Tuple ({ baz :: String } -> Aff Int) (VikeSpec Int (baz :: String)))
    :: Unit

class PathSegmentsToRecord :: Type -> Row Type -> Constraint
class PathSegmentsToRecord pathSegment record | pathSegment -> record where
  pathSegmentsToRecord :: List String -> Either ParseError { | record }

dummyPos = Position { column: 0, index: 0, line: 0 } :: Position

instance
  ( IsSymbol p
  , Rw.Cons p String r' r
  , Rw.Lacks p r'
  , PathSegmentsToRecord b r'
  ) =>
  PathSegmentsToRecord L.Nil () where
  pathSegmentsToRecord Nil = pure {}
  pathSegmentsToRecord l = Left
    (ParseError ("Too many path segments: " <> show l) dummyPos)

instance
  ( IsSymbol p
  , Rw.Cons p String r' r
  , Rw.Lacks p r'
  , PathSegmentsToRecord b r'
  ) =>
  PathSegmentsToRecord (L.Cons (WildcardPath (Proxy p)) b) r where
  pathSegmentsToRecord Nil = Left
    (ParseError "Not enough path segments" dummyPos)
  pathSegmentsToRecord (h : t) = do
    rest <- pathSegmentsToRecord @b t
    pure $ Rec.insert (Proxy :: Proxy p) h rest

instance
  ( IsSymbol p
  , Rw.Cons p String r' r
  , Rw.Lacks p r'
  , PathSegmentsToRecord b r'
  ) =>
  PathSegmentsToRecord (L.Cons (LiteralPath (Proxy p)) b) r where
  pathSegmentsToRecord Nil = Left
    (ParseError "Not enough path segments" dummyPos)
  pathSegmentsToRecord (h : t)
    | h == reflectSymbol (Proxy :: Proxy p) = do
        rest <- pathSegmentsToRecord @b t
        pure $ Rec.insert (Proxy :: Proxy p) h rest
    | otherwise = Left
        ( ParseError
            ( "Literal path segment does not match: "
                <> reflectSymbol (Proxy :: Proxy p)
                <> " "
                <> show h
            )
            dummyPos
        )

