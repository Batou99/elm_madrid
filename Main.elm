module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

-- MODEL

type alias Tema = {
  titulo : String,
  duracion : Int,
  id: Int
}


nuevoTema : String -> Int -> Int -> Tema
nuevoTema titulo duracion id= {
  titulo = titulo,
  duracion = duracion,
  id = id
  }

-- VIEW


pageHeader : Html msg
pageHeader =
  h1 [] [text "Temario"]


pageFooter : Html msg
pageFooter =
  footer []
    [a [href "https://github.com/Batou99/elm_madrid"]
       [text "Generador de temarios"]
    ]


capitulo : Tema -> Html msg
capitulo cap =
  li []
    [ span [class "titulo"] [text cap.titulo],
      span [class "duracion"] [text (toString cap.duracion)]
    ]


capitulos : Html msg
capitulos =
  ul [] [
    capitulo (nuevoTema "Introduccion" 5 1),
    capitulo (nuevoTema "Cierre" 4 2)]


view : Html msg
view =
  div [id "container"] [pageHeader, capitulos, pageFooter]


main : Html msg
main = 
  view
