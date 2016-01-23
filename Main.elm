module Main where

import Html exposing (..)
import Html.Attributes exposing (..)

pageHeader =
  h1 [] [text "Temario"]


pageFooter =
  footer []
    [a [href "https://github.com/Batou99/elm_madrid"]
       [text "Generador de temarios"]
    ]


main = 
  div [id "container"] [pageHeader, pageFooter]
