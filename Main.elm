module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

pageHeader =
  h1 [] [text "Temario"]


pageFooter =
  footer []
    [a [href "https://github.com/Batou99/elm_madrid"]
       [text "Generador de temarios"]
    ]


capitulos =
  ul [] [
    li [] [text "Introduccion"]]


view =
  div [id "container"] [pageHeader, capitulos, pageFooter]

main = 
  view
