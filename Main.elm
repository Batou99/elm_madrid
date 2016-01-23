module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

-- MODEL

type alias Tema = {
  titulo : String,
  duracion : Int,
  id: Int
}

type alias Model = List Tema

nuevoTema : String -> Int -> Int -> Tema
nuevoTema titulo duracion id= {
  titulo = titulo,
  duracion = duracion,
  id = id
  }


modeloInicial : Model
modeloInicial = [
  nuevoTema "01. Bienvenida" 5 1,
  nuevoTema "99. Cierre" 4 2,
  nuevoTema "02. Introduccion" 6 2
  ]

-- UPDATE


type Action = NoOp | SortByTitulo | SortByDuracion


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    SortByTitulo ->
      List.sortBy .titulo model
    SortByDuracion ->
      List.sortBy .duracion model


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


view : Model -> Html msg
view model =
  div [id "container"]
    [pageHeader, 
    ul [] (List.map capitulo model),
    pageFooter]


main : Html msg
main = 
  modeloInicial
    |> update SortByTitulo
    |> view
