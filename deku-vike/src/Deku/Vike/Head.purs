module Deku.Vike.Head where

import Prelude

import Data.Array (foldMap)
import Data.Maybe (Maybe(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (class Newtype, un)
import Data.String (joinWith)



-- Define types for scripts, links, and base tag
newtype Script = Script
  { src :: String
  , async :: Disj Boolean
  , defer ::Disj Boolean
  , type_ :: Maybe String
  , integrity :: Maybe String
  , crossorigin :: Maybe String
  }

newtype Link = Link
  { href :: String
  , rel :: String
  , type_ :: Maybe String
  , media :: Maybe String
  , sizes :: Maybe String
  , integrity :: Maybe String
  , crossorigin :: Maybe String
  }

newtype Base = Base
  { href :: Maybe String
  , target :: Maybe String
  }

-- Define the overall Head record
newtype Head = Head
  { title :: Maybe String
  , description :: Maybe String
  , scripts :: Array Script
  , links :: Array Link
  , base :: Maybe Base
  , meta :: Array { name :: Maybe String, content :: String }
  , charset :: Maybe String
  , viewport :: Maybe String
  , otherTags :: Array String
  }

derive instance newtypeScript :: Newtype Script _
derive instance newtypeLink :: Newtype Link _
derive instance newtypeBase :: Newtype Base _
derive instance newtypeHead :: Newtype Head _

derive newtype instance eqScript :: Eq Script
derive newtype instance eqLink :: Eq Link
derive newtype instance eqBase :: Eq Base
derive newtype instance eqHead :: Eq Head

derive newtype instance ordScript :: Ord Script
derive newtype instance ordLink :: Ord Link
derive newtype instance ordBase :: Ord Base
derive newtype instance ordHead :: Ord Head

derive newtype instance showScript :: Show Script
derive newtype instance showLink :: Show Link
derive newtype instance showBase :: Show Base
derive newtype instance showHead :: Show Head

derive newtype instance semigroupScript :: Semigroup Script
derive newtype instance semigroupLink :: Semigroup Link
derive newtype instance semigroupBase :: Semigroup Base
derive newtype instance semigroupHead :: Semigroup Head

derive newtype instance monoidScript :: Monoid Script
derive newtype instance monoidLink :: Monoid Link
derive newtype instance monoidBase :: Monoid Base
derive newtype instance monoidHead :: Monoid Head


-- Convert a Script record to an HTML <script> tag
scriptToString :: Script -> String
scriptToString (Script s) =
  let
    asyncAttr = if (un Disj s.async) then " async" else ""
    deferAttr = if (un Disj s.defer) then " defer" else ""
    typeAttr = case s.type_ of
      Just t -> " type=\"" <> t <> "\""
      Nothing -> ""
    integrityAttr = case s.integrity of
      Just i -> " integrity=\"" <> i <> "\""
      Nothing -> ""
    crossoriginAttr = case s.crossorigin of
      Just c -> " crossorigin=\"" <> c <> "\""
      Nothing -> ""
  in
    "<script src=\"" <> s.src <> "\"" <> asyncAttr <> deferAttr <> typeAttr <> integrityAttr <> crossoriginAttr <> "></script>"

-- Convert a Link record to an HTML <link> tag
linkToString :: Link -> String
linkToString (Link l) =
  let
    typeAttr = case l.type_ of
      Just t -> " type=\"" <> t <> "\""
      Nothing -> ""
    mediaAttr = case l.media of
      Just m -> " media=\"" <> m <> "\""
      Nothing -> ""
    sizesAttr = case l.sizes of
      Just s -> " sizes=\"" <> s <> "\""
      Nothing -> ""
    integrityAttr = case l.integrity of
      Just i -> " integrity=\"" <> i <> "\""
      Nothing -> ""
    crossoriginAttr = case l.crossorigin of
      Just c -> " crossorigin=\"" <> c <> "\""
      Nothing -> ""
  in
    "<link href=\"" <> l.href <> "\" rel=\"" <> l.rel <> "\"" <> typeAttr <> mediaAttr <> sizesAttr <> integrityAttr <> crossoriginAttr <> ">"

-- Convert a Base record to an HTML <base> tag
baseToString :: Base -> String
baseToString (Base b) =
  let
    hrefAttr = case b.href of
      Just h -> " href=\"" <> h <> "\""
      Nothing -> ""
    targetAttr = case b.target of
      Just t -> " target=\"" <> t <> "\""
      Nothing -> ""
  in
    "<base" <> hrefAttr <> targetAttr <> ">"

-- Convert meta tags to strings
metaToString :: { name :: Maybe String, content :: String } -> String
metaToString m =
  case m.name of
    Just n -> "<meta name=\"" <> n <> "\" content=\"" <> m.content <> "\">"
    Nothing -> "<meta content=\"" <> m.content <> "\">"

-- Convert the entire Head record to an HTML string
headToString :: Head -> String
headToString (Head h) =
  let
    titleTag = case h.title of
      Just t -> "<title>" <> t <> "</title>"
      Nothing -> ""

    descriptionTag = case h.description of
      Just d -> "<meta name=\"description\" content=\"" <> d <> "\">"
      Nothing -> ""

    charsetTag = case h.charset of
      Just c -> "<meta charset=\"" <> c <> "\">"
      Nothing -> ""

    viewportTag = case h.viewport of
      Just v -> "<meta name=\"viewport\" content=\"" <> v <> "\">"
      Nothing -> ""

    baseTag = case h.base of
      Just b -> baseToString b
      Nothing -> ""

    scriptTags = foldMap scriptToString h.scripts
    linkTags = foldMap linkToString h.links
    metaTags = foldMap metaToString h.meta
    otherTagStrings = joinWith "\n" h.otherTags
  in
    "<head>\n" <>
      charsetTag <> "\n" <>
      titleTag <> "\n" <>
      descriptionTag <> "\n" <>
      viewportTag <> "\n" <>
      baseTag <> "\n" <>
      linkTags <> "\n" <>
      metaTags <> "\n" <>
      scriptTags <> "\n" <>
      otherTagStrings <>
    "\n</head>"
