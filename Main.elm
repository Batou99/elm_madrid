module Main where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StartApp.Simple

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


pageHeader : Html
pageHeader =
  h1 [] [text "Temario"]


pageFooter : Html
pageFooter =
  footer []
    [a [href "https://github.com/Batou99/elm_madrid"]
       [text "Generador de temarios"]
    ]


capitulo : Tema -> Html
capitulo cap =
  li []
    [ span [class "titulo"] [text cap.titulo],
      span [class "duracion"] [text (toString cap.duracion)]
    ]


capitulos : List Tema -> Html
capitulos temas =
  ul [] (List.map capitulo temas)


view : Signal.Address Action -> Model -> Html
view address model =
  div [id "container"]
    [pageHeader, 
    button
      [class "sort left", onClick address SortByTitulo]
      [text "Titulo"],
    button
      [class "sort", onClick address SortByDuracion]
      [text "Duracion"],
    capitulos model,
    pageFooter]


main : Signal Html
main = 
  StartApp.Simple.start
    { model = update SortByTitulo modeloInicial,
      view = view,
      update = update
    }
