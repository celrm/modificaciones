module View exposing (view)

import Html exposing (..)
import Html.Events exposing (on, onClick, onInput, onFocus)
import Html.Attributes exposing (..)
import Json.Decode
import Http

import Info exposing (informacion)
import Functions exposing (modify)
import Types exposing (..)
import Styles exposing (..)


doblecolumna : String -> List (Html Msg) -> List (Html Msg) -> Html Msg
doblecolumna s a b =
  div
    [ style [ ("padding",
              ( s ++ "px 0px "
              ++ (toString <| (\x->2*x) <| Result.withDefault 0 <| String.toInt s)
              ++ "px 0px")
              )
            ]
    ]
    [ div [ style [ ("float", "left"), ("width", "50%") ] ]
      [ div [ style [ ("float", "right"), ("width", "400px") ] ]
        a
      ]
    , div [ style [ ("float", "right"), ("width", "50%") ] ]
      [ div [ style [ ("float", "left"), ("width", "400px") ] ]
        b
      ]
    ]


header : Html Msg
header =
    h1
      [ textStyle "2em" ]
      [ text "MODIFICACIÓN DE PARTITURAS" ]


selectors : Bool -> Html Msg
selectors bloqueado =
  doblecolumna "100"
    [ label
        [ textStyle "20px" ]
        [ input
          [ type_ "radio"
          , name "Escala"
          , onClick (Preescalar "Pentafónica" 0 0 2 2 4 4 7 7 7 9 9 12)
          ] []
        , text " Pentafónica"
        ]
      , br [] [], br [] []
      , label
        [ textStyle "20px" ]
        [ input
          [ type_ "radio"
          , name "Escala"
          , onClick (Preescalar "Hexafónica" 0 0 2 2 4 4 6 6 8 8 10 10)
          ] []
        , text " Hexafónica"
        ]
    ]
    [ label
        [ textStyle "20px" ]
        [ input
          [ type_ "radio"
          , name "Escala"
          , onClick (Preescalar "Heptafónica" 0 0 2 2 4 5 5 7 7 9 9 11)
          ] []
        , text " Heptafónica"
        ]
      , br [] [], br [] []
      , label
        [ textStyle "20px" ]
        [ input
          [ type_ "radio"
          , name "Escala"
          , checked (not bloqueado)
          , onClick VolverAEscalaAnterior
          ] []
        , text " "
        ]
      , input
        [ squarefieldStyle "150px"
        , defaultValue "Cromática"
        , onInput CambiarNombreEscala
        , readonly bloqueado
        ] []
    ]

entrada : Bool -> Html Msg
entrada bloqueado =
  div
    [ style [("margin-bottom","50px"), ("overflow", "auto")] ]
    [ table
      [ style
        [ ("margin-left","auto")
        , ("margin-right","auto")
        ]
      ]
      [ tr []
        ( List.map
          ( \x-> td [ cellStyle, style
              [ ("text-align", "center")
              , ("width", "42px")
              , ("font-family","calibri")
              , ("font-size", "20px")
              , ("padding", "3.5px 2px")
              ] ] [text x] ) <| ["Do","Do#","Re","Re#","Mi","Fa","Fa#","Sol","Sol#","La","La#","Si"] --toString<|List.range 0 11
        )
      , tr []
        ( List.repeat 12
          ( td [ style
              [ ("text-align", "center")
              , ("width", "42px")
              , ("font-family","calibri")
              , ("font-size", "20px")
              , ("padding", "3.5px 2px")
              , ("border", "0px none transparent")
              ] ] [text "↓"]
          )
        )
    , tr []
      [ recuadrin bloqueado "0" Do
      , recuadrin bloqueado "1" Dos
      , recuadrin bloqueado "2" Re
      , recuadrin bloqueado "3" Res
      , recuadrin bloqueado "4" Mi
      , recuadrin bloqueado "5" Fa
      , recuadrin bloqueado "6" Fas
      , recuadrin bloqueado "7" Sol
      , recuadrin bloqueado "8" Sols
      , recuadrin bloqueado "9" La
      , recuadrin bloqueado "10" Las
      , recuadrin bloqueado "11" Si
      ]
    ]
  ]

recuadrin : Bool -> String -> Nota -> Html Msg
recuadrin b s n = td [] [input
  [ squarefieldStyle "30px"
  , defaultValue s
  , onInput (Introducir n)
  , readonly b
  ]
  []]


examinar : Model -> Html Msg
examinar model =
  input
    [ style [("margin-top","50px")]
    , fileStyle
    , type_ "file"
    , id model.id
    , accept ".mscx"
    , on "change"
      (Json.Decode.succeed ScoreSelected)
    ]
    []


descargar : Model -> Html Msg
descargar model =
    let scoreDownload =
      case model.mScore of
        Just i ->
          i
        Nothing ->
          example
    in div []
        [ a
          [ type_ "button"
          , href <| "data:text/plain;charset=utf-8," ++ Http.encodeUri scoreDownload.contents
          , downloadAs scoreDownload.filename
          ]
          [ button
            [ buttonStyle "150px"
            , style [("margin-top","50px"),("margin-bottom","50px")]
            ] [ text "Descargar" ] ]
        ]

view : Model -> Html Msg
view model =

  div [ generalStyle ]
    [ header
    , examinar model
    , selectors model.bloqueado
    , entrada model.bloqueado
    , descargar (modify model)
    , informacion
    ]
