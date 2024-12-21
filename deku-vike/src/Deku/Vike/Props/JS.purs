module Deku.Vike.Props.JS where

import Data.Nullable (Nullable)
import Data.Newtype (class Newtype)

import Deku.DOM (HTMLHyperlinkElementUtils, HTMLElement)

import Foreign.Object (Object)
import Type.Proxy (Proxy)
import Deku.Vike.Props as VP

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

newtype PreviousPageContext :: Row Type -> Type
newtype PreviousPageContext record =
  PreviousPageContext
    ( forall path data'
       . VP.PreviousPageContextSignature path record data'
      => Proxy path
      -> Nullable (PageContext data' record)
    )

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
  , previousPageContext :: PreviousPageContext r
  }

derive instance Newtype (PageContext a r) _

newtype VikeProps a r = VikeProps
  { pageContext :: PageContext a r
  , clientOnly :: VP.ClientOnly
  , nav :: VP.Nav r
  }

derive instance Newtype (VikeProps a r) _