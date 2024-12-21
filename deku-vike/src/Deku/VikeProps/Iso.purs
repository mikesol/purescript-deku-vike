module Deku.VikeProps.Iso where

import Prelude

import Data.List as List
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Data.Tuple (Tuple(..))
import Deku.DOM (p)
import Deku.Vike.VikeProps as VP
import Deku.Vike.VikeProps.JS as VP.JS
import Foreign.Object as FO
import Type.Equality (class TypeEquals)

pageContextToPageContextJS :: forall data' record. VP.PageContext data' record -> VP.JS.PageContext data' record
pageContextToPageContextJS (VP.PageContext
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
          }) = VP.JS.PageContext
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
      , previousPageContext: ?hole -- VP.JS.PreviousPageContext (toNullable $ map pageContextToPageContextJS previousPageContext)
      -- , previousPageContext: (VP.JS.PreviousPageContext (toNullable $ map pageContextToPageContextJS (previousPageContext :: forall path data'0
      --  . VP.PreviousPageContextSignature path record data'0
      -- => Maybe (VP.PageContext data'0 record) ) ) ) :: VP.JS.PreviousPageContext record
      }
    where
    -- ppc :: VP.JS.PreviousPageContext record
    -- ppc = VP.JS.PreviousPageContext (toNullable $ map pageContextToPageContextJS (previousPageContext :: forall path data'0
    --    . VP.PreviousPageContextSignature path record data'0
    --   => Maybe (VP.PageContext data'0 record) ) )
    ppc :: forall @path0 data'0
       . VP.PreviousPageContextSignature path0 record data'0
      => Nullable (VP.JS.PageContext data'0 record)
    ppc = toNullable $ map pageContextToPageContextJS (previousPageContext @path0)
    ppc' :: VP.JS.PreviousPageContext record
    ppc' = VP.JS.PreviousPageContext ppc
    

vikePropsToVikePropsJS :: forall a r. VP.VikeProps a r -> VP.JS.VikeProps a r
vikePropsToVikePropsJS
  ( VP.VikeProps
      { pageContext
      , clientOnly
      , nav: VP.Nav { server, client }
      }
  ) = VP.JS.VikeProps
  { pageContext: pageContextToPageContextJS pageContext
  , clientOnly
  , nav: VP.JS.Nav { server, client }
  }