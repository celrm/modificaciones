module View exposing (view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Info exposing (informacion)
import Json.Decode as D
import Styles exposing (..)
import Types exposing (..)
import File exposing (File)


dragNdrop : Model -> Html Msg
dragNdrop model =
  div
    ( dropStyle model.hover ++
      [ hijackOn "dragenter" (D.succeed DragEnter)
      , hijackOn "dragover" (D.succeed DragEnter)
      , hijackOn "dragleave" (D.succeed DragLeave)
      , hijackOn "drop" dropDecoder
      ]
    )
    ([ button
        ( buttonStyle "150px" ++
          [ onClick SelectScores
          , style "margin" "20px" ]
        )
        [ text "Examinar" ]
    ] ++
      ( List.map
        (\score ->
          span
            ([ style "color" "#ccc" ] ++ textStyle "1em")
            [ text (.filename score) ]
        )
        model.scores
      )
    )

dropDecoder : D.Decoder Msg
dropDecoder =
  D.at ["dataTransfer","files"] (D.oneOrMore GotFiles File.decoder)

hijackOn : String -> D.Decoder msg -> Attribute msg
hijackOn event decoder =
  preventDefaultOn event (D.map hijack decoder)

hijack : msg -> (msg, Bool)
hijack msg =
  (msg, True)


opciones : Bool -> Html Msg
opciones bloqueado =
    doblecolumna "100"
        [ label
            ( textStyle "20px" )
            [ input
                [ type_ "radio"
                , name "Escala"
                , onClick (Preescalar "Pentafónica" 0 0 2 2 4 4 7 7 7 9 9 12)
                ]
                []
            , text " Pentafónica"
            ]
        , br [] []
        , br [] []
        , label
            ( textStyle "20px" )
            [ input
                [ type_ "radio"
                , name "Escala"
                , onClick (Preescalar "Hexafónica" 0 0 2 2 4 4 6 6 8 8 10 10)
                ]
                []
            , text " Hexafónica"
            ]
        ]
        [ label
            ( textStyle "20px" )
            [ input
                [ type_ "radio"
                , name "Escala"
                , onClick (Preescalar "Heptafónica" 0 0 2 2 4 5 5 7 7 9 9 11)
                ]
                []
            , text " Heptafónica"
            ]
        , br [] []
        , br [] []
        , label
            ( textStyle "20px" )
            [ input
                [ type_ "radio"
                , name "Escala"
                , checked (not bloqueado)
                , onClick VolverAEscalaAnterior
                ]
                []
            , text " "
            ]
        , input
            ( squarefieldStyle "150px"
            ++ [ placeholder "Cromática"
            , onInput CambiarNombreEscala
            , readonly bloqueado
            ])
            []
        ]


entradaIndiv : Bool -> Html Msg
entradaIndiv bloqueado =
    div
        [ style "margin-bottom" "50px", style "overflow" "auto" ]
        [ table
            centered
            [ tr []
                (List.map
                    (\x ->
                        td
                            ( cellStyle
                            ++ [ style "text-align" "center"
                            , style "width" "42px"
                            , style "padding" "3.5px 2px"
                            ]
                            ++ (textStyle "20px")
                            )
                            [ text x ]
                    )
                 <|
                    [ "Do", "Do#", "Re", "Re#", "Mi", "Fa", "Fa#", "Sol", "Sol#", "La", "La#", "Si" ]
                 -- toString<|List.range 0 11
                )
            , tr []
                (List.repeat 12
                    (td
                        ([ style "text-align" "center"
                        , style "width" "42px"
                        , style "padding" "3.5px 2px"
                        , style "border" "0px none transparent"
                        ]
                        ++ (textStyle "20px")
                        )
                        [ text "↓" ]
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
recuadrin b s n =
    td []
        [ input
            ( squarefieldStyle "30px"
            ++ [ readonly b
            , placeholder s
            , onInput (Introducir n)
            ])
            []
        ]


descargar : Html Msg
descargar =
  div []
    [ button
        ( buttonStyle "150px" ++
          [ style "margin" "0px 0px 50px 0px"
          , onClick Descargar
          ]
        )
      [ text "Descargar" ]
    ]


view : Model -> Browser.Document Msg
view model =
  (Browser.Document "Modificaciones"
    [ div generalStyle
        [ navbar 3
        , titulo "MODIFICACIÓN DE PARTITURAS"
        , dragNdrop model
        , opciones model.bloqueado
        , entradaIndiv model.bloqueado
        , descargar
        , informacion
        ]
    ]
  )
