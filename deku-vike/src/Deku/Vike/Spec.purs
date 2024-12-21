module Deku.Vike.Spec where

import Prelude

import Deku.Core (Nut)
import Deku.Internal.Ancestry (Ancestry)
import Deku.Toplevel (ssrInBody)
import Deku.Vike.Head (Head, headToString)
import Deku.Vike.Props (VikeProps)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

newtype VikeSpec a r = VikeSpec
  { server :: VikeProps a r -> Aff String
  , client :: VikeProps a r -> Aff Nut
  }

templatingSpec
  :: forall a r
   . { builder :: VikeProps a r -> String -> String
     , spec :: VikeProps a r -> Nut
     }
  -> VikeSpec a r
templatingSpec { builder, spec } = VikeSpec
  { server: \vp -> liftEffect do
      buildHtml (builder vp) (ssrInBody (spec vp))
  , client: map pure spec
  }

headSpec
  :: forall a r
   . { builder :: VikeProps a r -> Head
     , spec :: VikeProps a r -> Nut
     }
  -> VikeSpec a r
headSpec { builder, spec } = templatingSpec { builder:
  \vp html -> "<!DOCTYPE html>\n<html>" <> headToString (builder vp) <> "<body>" <> html <> "</body>" <> "</html>"
  , spec
  }

foreign import buildHtml
  :: (String -> String)
  -> Effect
       { boring ∷ Array Ancestry, html ∷ String, livePortals ∷ Array Ancestry }
  -> Effect String