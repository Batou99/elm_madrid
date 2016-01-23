module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


pageHeader : Html msg
pageHeader =
  h1 [] [text "Temario"]


pageFooter : Html msg
pageFooter =
  footer []
    [a [href "https://github.com/Batou99/elm_madrid"]
       [text "Generador de temarios"]
    ]


capitulo : String -> Int -> Html msg
capitulo titulo duracion =
  li []
    [ span [class "titulo"] [text titulo],
      span [class "duracion"] [text (toString duracion)]
    ]


capitulos : Html msg
capitulos =
  ul [] [
    capitulo "Introduccion" 5]


view : Html msg
view =
  div [id "container"] [pageHeader, capitulos, pageFooter]


main : Html msg
main = 
  view
