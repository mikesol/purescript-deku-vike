module Deku.Vike where

a = 1

{-


vike
  { "/": dataHome /\ pageHome
  , "/user/:id": dataUser /\ pageUser
  , "/about": noData pageAbout
  }

newtype VikeProps a = VikeProps {pageContext :: PageContext a, clientOnly :: ClientOnly, nav :: Nav } 
pageUser :: VikeProps DataUser -> Nut

-}