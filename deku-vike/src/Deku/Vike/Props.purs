module Deku.Vike.Props where

import Data.List (List)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Deku.Attribute (Attribute) as Deku.Attribute
import Deku.Core (Nut)
import Deku.DOM (HTMLHyperlinkElementUtils, HTMLElement)
import FRP.Poll (Poll)
import FRP.Poll as FRP.Poll
import Type.Proxy (Proxy)

newtype UrlParsed = UrlParsed
  { pathname :: String
  , pathnameOriginal :: String
  , search :: Map String String
  , searchAll :: Map String (List String)
  , searchOriginal :: Maybe String
  , hash :: String
  , hashOriginal :: Maybe String
  , href :: String
  , origin :: Maybe String
  , protocol :: Maybe String
  , hostname :: Maybe String
  , port :: Maybe String
  }

type VikeAnchorElement (r :: Row Type) =
  ( __tag :: Proxy "VikeAnchorElement"
  , urn :: String
  , rev :: String
  , name :: String
  , methods :: String
  , shape :: String
  , coords :: String
  , charset :: String
  , referrerpolicy :: String
  , xtype :: String
  , hreflang :: String
  , rel :: String
  , ping :: String
  , download :: String
  , target :: String
  | HTMLHyperlinkElementUtils (HTMLElement r)
  )

type AnchorSignature =
  Array (FRP.Poll.Poll (Deku.Attribute.Attribute (VikeAnchorElement ())))
  -> Array Nut
  -> Nut

class ClientNavSignature :: Symbol -> Row Type -> Constraint
class ClientNavSignature path record

newtype ClientNav record =
  ClientNav
    ( forall path
       . ClientNavSignature path record
      => { | record }
      -> Proxy path
      -> AnchorSignature
    )

class ServerNavSignature :: Symbol -> Row Type -> Type -> Constraint
class ServerNavSignature path record data' | path record -> data'

newtype ServerNav :: Row Type -> Type
newtype ServerNav record =
  ServerNav
    ( forall path data'
       . ServerNavSignature path record data'
      => Proxy path
      -> Poll data'
      -> { | record }
      -> AnchorSignature
    )

class PreviousPageContextSignature :: Symbol -> Row Type -> Type -> Constraint
class
  PreviousPageContextSignature path record data'
  | path record -> data'

newtype PreviousPageContext :: Row Type -> Type
newtype PreviousPageContext record =
  PreviousPageContext
    ( forall path data'
       . PreviousPageContextSignature path record data'
      => Proxy path
      -> Maybe (PageContext data' record)
    )

newtype Nav r = Nav
  { server :: ServerNav r
  , client :: ClientNav r
  }

derive instance Newtype (Nav r) _

newtype PageContext :: Type -> Row Type -> Type
newtype PageContext a r = PageContext
  { data :: a
  , urlPathname :: String
  , urlOriginal :: String
  , urlParsed :: UrlParsed
  , headers :: Map String String
  , headersOriginal :: List (Tuple String String)
  , isHydration :: Boolean
  , isClientSideNavigation :: Boolean
  , previousPageContext :: PreviousPageContext r
  }

derive instance Newtype (PageContext a r) _
type ClientOnly = Nut -> Nut

newtype VikeProps a r = VikeProps
  { pageContext :: PageContext a r
  , clientOnly :: ClientOnly
  , nav :: Nav r
  }

derive instance Newtype (VikeProps a r) _