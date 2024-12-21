module Deku.Vike.Props.Iso where

import Prelude

import Data.Array as Array
import Data.Compactable (compact)
import Data.List as List
import Data.Map as DM
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe, toNullable)
import Data.Tuple (Tuple(..))
import Deku.Vike.Props as VP
import Deku.Vike.Props.JS as VP.JS
import Foreign.Object as FO

pageContextToPageContextJS
  :: forall data' record
   . VP.PageContext data' record
  -> VP.JS.PageContext data' record
pageContextToPageContextJS
  ( VP.PageContext
      { data: data'
      , urlPathname
      , urlOriginal
      , urlParsed: VP.UrlParsed
          { pathname
          , pathnameOriginal
          , search
          , searchAll
          , searchOriginal
          , hash
          , hashOriginal
          , href
          , origin
          , protocol
          , hostname
          , port
          }

      , headers
      , headersOriginal
      , isHydration
      , isClientSideNavigation
      , previousPageContext: VP.PreviousPageContext previousPageContext
      }
  ) = VP.JS.PageContext
  { data: data'
  , urlPathname
  , urlOriginal
  , urlParsed: VP.JS.UrlParsed
      { pathname
      , pathnameOriginal
      , search: FO.fromFoldableWithIndex search
      , searchAll: FO.fromFoldableWithIndex $ map List.toUnfoldable
          searchAll
      , searchOriginal: toNullable searchOriginal
      , hash
      , hashOriginal: toNullable hashOriginal
      , href
      , origin: toNullable origin
      , protocol: toNullable protocol
      , hostname: toNullable hostname
      , port: toNullable port
      }
  , headers: FO.fromFoldableWithIndex headers
  , headersOriginal: List.toUnfoldable $ map (\(Tuple a b) -> [ a, b ])
      headersOriginal
  , isHydration
  , isClientSideNavigation
  , previousPageContext: VP.JS.PreviousPageContext \px -> toNullable $ map
      pageContextToPageContextJS
      (previousPageContext px)
  }

vikePropsToVikePropsJS :: forall a r. VP.VikeProps a r -> VP.JS.VikeProps a r
vikePropsToVikePropsJS
  ( VP.VikeProps
      { pageContext
      , clientOnly
      , nav
      }
  ) = VP.JS.VikeProps
  { pageContext: pageContextToPageContextJS pageContext
  , clientOnly
  , nav
  }

pageContextJSToPageContext
  :: forall data' record
   . VP.JS.PageContext data' record
  -> VP.PageContext data' record
pageContextJSToPageContext
  ( VP.JS.PageContext
      { data: data'
      , urlPathname
      , urlOriginal
      , urlParsed: VP.JS.UrlParsed
          { pathname
          , pathnameOriginal
          , search
          , searchAll
          , searchOriginal
          , hash
          , hashOriginal
          , href
          , origin
          , protocol
          , hostname
          , port
          }

      , headers
      , headersOriginal
      , isHydration
      , isClientSideNavigation
      , previousPageContext: VP.JS.PreviousPageContext previousPageContext
      }
  ) = VP.PageContext
  { data: data'
  , urlPathname
  , urlOriginal
  , urlParsed: VP.UrlParsed
      { pathname
      , pathnameOriginal
      , search: DM.fromFoldableWithIndex search
      , searchAll: DM.fromFoldableWithIndex $ map Array.toUnfoldable
          searchAll
      , searchOriginal: toMaybe searchOriginal
      , hash
      , hashOriginal: toMaybe hashOriginal
      , href
      , origin: toMaybe origin
      , protocol: toMaybe protocol
      , hostname: toMaybe hostname
      , port: toMaybe port
      }
  , headers: DM.fromFoldableWithIndex headers
  , headersOriginal: Array.toUnfoldable $ compact $ map hf
      headersOriginal
  , isHydration
  , isClientSideNavigation
  , previousPageContext: VP.PreviousPageContext \px -> map
      pageContextJSToPageContext
      (toMaybe $ previousPageContext px)
  }
  where
  hf :: Array String -> Maybe (Tuple String String)
  hf [ a, b ] = Just $ Tuple a b
  hf _ = Nothing

vikePropsJSToVikeProps :: forall a r. VP.JS.VikeProps a r -> VP.VikeProps a r
vikePropsJSToVikeProps
  ( VP.JS.VikeProps
      { pageContext
      , clientOnly
      , nav
      }
  ) = VP.VikeProps
  { pageContext: pageContextJSToPageContext pageContext
  , clientOnly
  , nav
  }