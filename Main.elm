module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as Html

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
  nuevoTema "99. Cierre" 4 99,
  nuevoTema "02. Introduccion" 6 2
  ]

-- UPDATE


type Msg
  = NoOp
  | SortByTitulo
  | SortByDuracion
  | Delete Int


update : Msg -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    SortByTitulo ->
      List.sortBy .titulo model
    SortByDuracion ->
      List.sortBy .duracion model
    Delete id ->
      List.filter (\t -> t.id /= id) model


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


capitulo : Tema -> Html Msg
capitulo cap =
  li []
    [ span [class "titulo"] [text cap.titulo],
      span [class "duracion"] [text (toString cap.duracion)],
      button
        [class "delete", onClick (Delete cap.id)]
        []
    ]


capitulos : List Tema -> Html Msg
capitulos temas =
  ul [] (List.map capitulo temas)


view : Model -> Html Msg
view model =
  div [id "container"]
    [pageHeader, 
    button
      [class "sort left", onClick SortByTitulo]
      [text "Titulo"],
    button
      [class "sort", onClick SortByDuracion]
      [text "Duracion"],
    capitulos model,
    pageFooter]


main = 
  Html.beginnerProgram
    { model = update SortByTitulo modeloInicial,
      view = view,
      update = update
    }
