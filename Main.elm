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

type alias Model = {
  temas : List Tema,
  tituloInput : String,
  duracionInput : String,
  nextID : Int
}

nuevoTema : String -> Int -> Int -> Tema
nuevoTema titulo duracion id= {
  titulo = titulo,
  duracion = duracion,
  id = id
  }


modeloInicial : Model
modeloInicial = {
  temas = [
    nuevoTema "01. Bienvenida" 5 1,
    nuevoTema "99. Cierre" 4 99,
    nuevoTema "02. Introduccion" 6 2
  ],
  tituloInput = "",
  duracionInput = "",
  nextID = 3
  }


-- UPDATE


type Action
  = NoOp
  | SortByTitulo
  | SortByDuracion
  | Delete Int
  | UpdateTitulo String
  | UpdateDuracion String


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    SortByTitulo ->
      { model | temas = List.sortBy .titulo model.temas }
    SortByDuracion ->
      { model | temas = List.sortBy .duracion model.temas }
    Delete id ->
      { model | temas = List.filter (\t -> t.id /= id) model.temas }
    UpdateTitulo titulo ->
      { model | tituloInput = titulo }
    UpdateDuracion duracion ->
      { model | duracionInput = duracion }


-- VIEW


totalDuraciones : List Tema -> Int
totalDuraciones temas =
  let
      duraciones = List.map .duracion temas
  in
      List.foldl (+) 0 duraciones

pageHeader : Html
pageHeader =
  h1 [] [text "Temario"]


pageFooter : Html
pageFooter =
  footer []
    [a [href "https://github.com/Batou99/elm_madrid"]
       [text "Generador de temarios"]
    ]


capitulo : Signal.Address Action -> Tema -> Html
capitulo address cap =
  li []
    [ span [class "titulo"] [text cap.titulo],
      span [class "duracion"] [text (toString cap.duracion)],
      button
        [class "delete", onClick address (Delete cap.id)]
        []
    ]


capitulos : Signal.Address Action -> List Tema -> Html
capitulos address temas =
  let
      entradas = List.map (capitulo address) temas 
      elementos = entradas ++ [ muestraTotal (totalDuraciones temas) ]
  in
      ul [] elementos


muestraTotal : Int -> Html
muestraTotal total =
  li
    [class "total"]
    [ span [class "label"] [text "Total"],
      span [class "duracion"] [text (toString total)]
    ]


formularioDeEntrada : Signal.Address Action -> Model -> Html
formularioDeEntrada address model =
  div []
    [ input
      [ type' "text",
        placeholder "Titulo",
        value model.tituloInput,
        name "titulo",
        autofocus True,
        on "input" targetValue (Signal.message address << UpdateTitulo)
        ] [],
      input
      [ type' "text",
        placeholder "Duracion",
        value model.duracionInput,
        name "duracion",
        on "input" targetValue (\str -> Signal.message address (UpdateDuracion str))
        ] [],
      button [ class "add" ] [ text "+" ],
      h2 []
        [ text (model.tituloInput ++ " " ++ model.duracionInput) ]
    ]

 
view : Signal.Address Action -> Model -> Html
view address model =
  div [id "container"]
    [pageHeader, 
    formularioDeEntrada address model,
    capitulos address model.temas,
    button
      [class "sort left", onClick address SortByTitulo]
      [text "Titulo"],
    button
      [class "sort", onClick address SortByDuracion]
      [text "Duracion"],
    pageFooter]


main : Signal Html
main = 
  StartApp.Simple.start
    { model = update SortByTitulo modeloInicial,
      view = view,
      update = update
    }
