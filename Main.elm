module Main where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StartApp
import Result
import String
import Effects exposing (Effects, Never)
import Json.Decode as JsD exposing ((:=))
import Json.Encode as JsE
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
  id: Int,
  modo : Modo
}

type Modo = Add | Edit


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
  id = 0,
  modo = Add
  }


init : (Model, Effects Action)
init = (modeloInicial, findAll)


-- EFFECTS

temasDecoder : JsD.Decoder (List Tema)
temasDecoder =
  JsD.list temaDecoder


temaDecoder : JsD.Decoder Tema
temaDecoder =
  JsD.object3 Tema
    ("titulo" := JsD.string)
    ("duracion" := JsD.int)
    ("id" := JsD.int)


temaEncoder : Tema -> String
temaEncoder tema =
  let
      inner =
        JsE.object
          [ ("titulo", JsE.string tema.titulo),
            ("duracion", JsE.int tema.duracion) ]

      namespace: String -> JsE.Value
      namespace name =
        JsE.object [(name, inner)]
  in
      JsE.encode 0 <|
        namespace "tema"


baseUrl : String
baseUrl =
  "http://dock:9009/api/temas"


findAll : Effects Action
findAll =
  Http.get temasDecoder baseUrl
    |> Task.toMaybe
    |> Task.map SetTemas
    |> Effects.task


borrarTema : Maybe Tema -> Effects Action
borrarTema mTema =
  case mTema of
    Just tema ->
      Http.send Http.defaultSettings
        {
          verb = "DELETE",
          headers = 
            [ ( "Content-Type", "application/json" ),
              ( "Accept", "application/json" )
            ],
          url = baseUrl ++ "/" ++ (toString tema.id),
          body = Http.empty
        }
        |> Task.toMaybe
        |> Task.map TemaDeleted
        |> Effects.task
    Nothing -> Effects.none

crearTema : Tema -> Effects Action
crearTema tema =
  let
      body =
        Http.string (temaEncoder tema)
  in
      Http.send Http.defaultSettings
        {
          verb = "POST",
          headers = 
            [ ( "Content-Type", "application/json" ),
              ( "Accept", "application/json" )
            ],
          url = baseUrl,
          body = body
        }
        |> Http.fromJson temaDecoder
        |> Task.toMaybe
        |> Task.map TemaPosted
        |> Effects.task


actualizarTema: Tema -> Effects Action
actualizarTema tema =
  let
      body =
        Http.string (temaEncoder tema)
  in
      Http.send Http.defaultSettings
        {
          verb = "PUT",
          headers = 
            [ ( "Content-Type", "application/json" ),
              ( "Accept", "application/json" )
            ],
          url = baseUrl ++ "/" ++ (toString tema.id),
          body = body
        }
        |> Task.toMaybe
        |> Task.map TemaUpdated
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
  | TemaPosted (Maybe Tema)
  | TemaUpdated (Maybe Http.Response)
  | TemaDeleted (Maybe Http.Response)
  | Editar Tema
  | Aceptar
  | Cancelar


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
      let
          borrar = List.filter (\t -> t.id == id) model.temas
      in
          (model, borrarTema (List.head borrar))
    UpdateTitulo titulo ->
      ({ model | tituloInput = titulo }, Effects.none)
    UpdateDuracion duracion ->
      ({ model | duracionInput = duracion }, Effects.none)
    Nuevo ->
      let
          duracion = String.toInt model.duracionInput |> Result.withDefault 0
          tema = nuevoTema model.tituloInput duracion 0
          valido = validateModel model
      in
          case valido of
            True -> 
              ({ model | temas = model.temas ++ [tema], tituloInput = "",
              duracionInput = "" }, crearTema tema)
            False ->
              (model, findAll)
    SetTemas response ->
      case response of
        Just temas ->
          ({ model | temas = temas }, Effects.none)
        Nothing ->
          (model, Effects.none)
    TemaPosted response ->
      let
          filteredModel = { model | temas = List.filter (\t -> t.id /= 0) model.temas } 
      in
          case response of
            Just tema -> 
              ({ filteredModel | temas = filteredModel.temas ++ [tema] }, Effects.none)
            Nothing ->
              (filteredModel, Effects.none)
    TemaUpdated response ->
      case response of
        Just _ -> ({ model | tituloInput = "",
                             duracionInput = "",
                             id = 0,
                             modo = Add }, findAll)
        Nothing -> (model, Effects.none)
    TemaDeleted response ->
      case response of
        Just _ -> (model, findAll)
        Nothing -> (model, Effects.none)
    Editar tema -> ( { model | modo = Edit,
                               tituloInput = tema.titulo,
                               duracionInput = (toString tema.duracion),
                               id = tema.id
                             }, Effects.none)
    Aceptar ->
      let
          duracion = String.toInt model.duracionInput |> Result.withDefault 0
          tema = nuevoTema model.tituloInput duracion model.id
          valido = validateModel model
      in
          case valido of
            True -> 
              (model, actualizarTema tema)
            False ->
              (model, findAll)
    Cancelar -> ({ model | modo = Add, tituloInput = "", duracionInput = "" }, Effects.none)


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
  li [onClick address (Editar cap)]
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
      botones address model,
      h2 []
        [ text (model.tituloInput ++ " " ++ model.duracionInput) ]
    ]


botones : Signal.Address Action -> Model -> Html
botones address model =
  case model.modo of
    Add ->
      span [] [
        button [ class "add", onClick address Nuevo ] [ text "+" ] ]
    Edit ->
    span [] [ 
        button [ class "add small", onClick address Aceptar ] [ text "✔" ],
        button [ class "add small", onClick address Cancelar ] [ text "✘" ] ]

 
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
