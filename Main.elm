module Main where

import Html exposing (..)
import Html.Attributes exposing (..)


pageHeader : Html
pageHeader =
  h1 [] [text "Temario"]


pageFooter : Html
pageFooter =
  footer []
    [a [href "https://github.com/Batou99/elm_madrid"]
       [text "Generador de temarios"]
    ]


capitulo : String -> Int -> Html
capitulo titulo duracion =
  li []
    [ span [class "titulo"] [text titulo],
      span [class "duracion"] [text (toString duracion)]
    ]


capitulos : Html
capitulos =
  ul [] [
    capitulo "Introduccion" 5]


view : Html
view =
  div [id "container"] [pageHeader, capitulos, pageFooter]


main : Html
main = 
  view
