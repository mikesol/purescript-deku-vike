module Deku.Vike.VikeProps.JS where

import Data.Nullable (Nullable)
import Data.Newtype (class Newtype)
import Deku.Attribute (Attribute) as Deku.Attribute
import Deku.Core (Nut)
import Deku.DOM (HTMLHyperlinkElementUtils, HTMLElement)
import FRP.Poll (Poll)
import FRP.Poll as FRP.Poll
import Foreign.Object (Object)
import Type.Proxy (Proxy)
import Deku.Vike.VikeProps as VP

newtype UrlParsed = UrlParsed
  { pathname :: String
  , pathnameOriginal :: String
  , search :: Object String
  , searchAll :: Object (Array String)
  , searchOriginal :: Nullable String
  , hash :: String
  , hashOriginal :: Nullable String
  , href :: String
  , origin :: Nullable String
  , protocol :: Nullable String
  , hostname :: Nullable String
  , port :: Nullable String
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

newtype ClientNav record = 
    ClientNav (forall @path. VP.ClientNavSignature path record => { | record } -> AnchorSignature)

newtype ServerNav :: Row Type ->  Type
newtype ServerNav record  = 
    ServerNav (forall @path data'. VP.ServerNavSignature path record data' => Poll data' -> { | record } -> AnchorSignature)


newtype PreviousPageContext :: Row Type ->  Type
newtype PreviousPageContext record  = 
    PreviousPageContext (forall @path data'. VP.PreviousPageContextSignature path record data' => Nullable (PageContext data' record) )


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
  , headers :: Object String
  , headersOriginal :: Array (Array String)
  , isHydration :: Boolean
  , isClientSideNavigation :: Boolean
  , previousPageContext  :: PreviousPageContext r
  }

derive instance Newtype (PageContext a r) _
newtype ClientOnly = ClientOnly (Nut -> Nut)

derive instance Newtype ClientOnly _
newtype VikeProps a r = VikeProps
  { pageContext :: PageContext a r, clientOnly :: ClientOnly, nav :: Nav r }

derive instance Newtype (VikeProps a r) _