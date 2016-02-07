module Main where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StartApp
import Result
import String
import Effects exposing (Effects, Never)
import Json.Decode as Js exposing ((:=))
import Http
import Task exposing (Task)
import Maybe exposing (Maybe)

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
  temas = [],
  tituloInput = "",
  duracionInput = "",
  nextID = 3
  }


init : (Model, Effects Action)
init = (modeloInicial, findAll)


-- EFFECTS

temasDecoder : Js.Decoder (List Tema)
temasDecoder =
  Js.list temaDecoder


temaDecoder : Js.Decoder Tema
temaDecoder =
  Js.object3 Tema
    ("titulo" := Js.string)
    ("duracion" := Js.int)
    ("id" := Js.int)


findAll : Effects Action
findAll =
  Http.get temasDecoder "temas.json"
    |> Task.toMaybe
    |> Task.map SetTemas
    |> Effects.task

-- UPDATE


type Action
  = NoOp
  | SortByTitulo
  | SortByDuracion
  | Delete Int
  | UpdateTitulo String
  | UpdateDuracion String
  | Nuevo
  | SetTemas (Maybe (List Tema))


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
      (model, Effects.none)
    SortByTitulo ->
      ({ model | temas = List.sortBy .titulo model.temas }, Effects.none)
    SortByDuracion ->
      ({ model | temas = List.sortBy .duracion model.temas }, Effects.none)
    Delete id ->
      ({ model | temas = List.filter (\t -> t.id /= id) model.temas },
      Effects.none)
    UpdateTitulo titulo ->
      ({ model | tituloInput = titulo }, Effects.none)
    UpdateDuracion duracion ->
      ({ model | duracionInput = duracion }, Effects.none)
    Nuevo ->
      let
          duracion = String.toInt model.duracionInput |> Result.withDefault 0
          tema = nuevoTema model.tituloInput duracion (model.nextID)
          valido = validateModel model
      in
          case valido of
            True -> 
              ({ model | temas = model.temas ++ [tema], tituloInput = "",
              duracionInput = "" }, Effects.none)
            False ->
              (model, Effects.none)
    SetTemas response ->
      case response of
        Just temas ->
          ({ model | temas = temas }, Effects.none)
        Nothing ->
          (model, Effects.none)


validateModel : Model -> Bool
validateModel model =
  let
      tituloValido = not (String.isEmpty model.tituloInput)
      duracionValida = case (String.toInt model.duracionInput) of
                         (Err _ ) -> False
                         _        -> True
  in
      tituloValido && duracionValida
  
                         


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
      button [ class "add", onClick address Nuevo ] [ text "+" ],
      h2 []
        [ text (model.tituloInput ++ " " ++ model.duracionInput) ]
    ]

 
view : Signal.Address Action -> Model -> Html
view address model =
  div [id "container"]
    [pageHeader, 
    formularioDeEntrada address model,
    br [] [],
    button
      [class "sort left", onClick address SortByTitulo]
      [text "Titulo"],
    button
      [class "sort", onClick address SortByDuracion]
      [text "Duracion"],
    br [] [],
    capitulos address model.temas,
    pageFooter]


main : Signal Html
main = 
  app.html


app = 
  StartApp.start
    { 
      init = init,
      view = view,
      update = update,
      inputs = []
    }


port tasks : Signal (Task Never ())
port tasks =
  app.tasks
