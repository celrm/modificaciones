module Styles exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style,href,placeholder)
import Html.Events exposing (onInput,onClick)

back : String
back = "#F9F9FF"

suave : String
suave = "#EEEEFF"

bold : String
bold = "#BBBBFF"

generalStyle : List (Attribute msg)
generalStyle =
    [ style "text-align" "center"
    , style "background-color" back
    , style "overflow" "auto"
    ]


textStyle : String -> List (Attribute msg)
textStyle tam =
    [ style "font-family" "calibri"
    , style "font-size" tam
    ]


cellStyle : List (Attribute msg)
cellStyle =
    [ style "text-align" "center"
    , style "width" "50px"
    , style "max-width" "50px"
    , style "min-width" "50px"
    , style "border" "1px solid black"
    , style "font-family" "calibri"
    , style "background-color" suave
    ]


boldStyle : List (Attribute msg)
boldStyle =
    [ style "text-align" "center"
    , style "width" "50px"
    , style "max-width" "50px"
    , style "min-width" "50px"
    , style "border" "3px solid black"
    , style "font-weight" "bold"
    , style "font-family" "calibri"
    , style "background-color" bold
    , style "box-shadow" "5px 7.5px"
    ]
--  ++ noSelect


noSelect : List (Attribute msg)
noSelect =
    [ style "-webkit-user-select" "none"
    , style "-khtml-user-select" "none"
    , style "-moz-user-select" "none"
    , style "-o-user-select" "none"
    , style "user-select" "none"
    ]


tableStyle : List (Attribute msg)
tableStyle =
    [ style "text-align" "center"
    , style "border-collapse" "collapse"
    , style "table-layout" "fixed"
    , style "margin" "20px 0px"
    ]
    ++ centered


centered : List (Attribute msg)
centered =
  [ style "margin-left" "auto"
  , style "margin-right" "auto"
  ]


-- selectStyle : List (Attribute msg)
-- selectStyle =
--     [ style "width" "208px"
--     , style "outline" "none"
--     , style "background-image" "urlstyle http://i62.tinypic.com/15xvbd5.png"
--     ]
--     ++ weird
--     ++ ( textStyle "20px" )


buttonStyle : String -> List (Attribute msg)
buttonStyle s =
    [ style "width" s ]
    ++ ( textStyle "20px" )


fileStyle : List (Attribute msg)
fileStyle =
    weird
    ++ ( textStyle "20px" )
    ++ centered

weird : List (Attribute msg)
weird =
    [ style "padding" "3.5px 10px"
    , style "appearance" "none"
    , style "-moz-appearance" "none"
    , style "-webkit-appearance" "none"
    , style "background-repeat" "no-repeat"
    , style "background-position" "95%"
    , style "border" "1px solid #88888"
    , style "border-radius" "20px 20px 20px 20px"
    , style "color" "black"
    , style "overflow" "hidden"
    , style "-moz-border-radius" "20px 20px 20px 20px"
    , style "-webkit-border-radius" "20px 20px 20px 20px"
    ]


littlebuttonStyle : List (Attribute msg)
littlebuttonStyle =
    [ style "width" "50px" ]
    ++ weird
    ++ ( textStyle "20px" )


fieldStyle : List (Attribute msg)
fieldStyle =
    [ style "width" "650px"
    , style "text-align" "center"
    , style "border-radius" "20px 20px 20px 20px"
    ]
    ++ ( textStyle "30px" )


squarefieldStyle : String -> List (Attribute msg)
squarefieldStyle s =
    [ style "width" s
    , style "text-align" "center"
    , style "padding" "3.5px 2px"
    ]
    ++ ( textStyle "20px" )

doblecolumna : String -> List (Html msg) -> List (Html msg) -> Html msg
doblecolumna s a b =
  let tamancho = "400px"
  in
    div [ style "padding"
            (s
            ++ "px 0px "
            ++ (s |> String.toInt |> Maybe.withDefault 0 |> (*) 2 |> String.fromInt)
            ++ "px 0px"
            )
        ]
        [ div [ style "float" "left", style "width" "50%" ]
            [ div [ style "float" "right", style "width" tamancho ]
                a
            ]
        , div [ style "float" "right", style "width" "50%" ]
            [ div [ style "float" "left", style "width" tamancho ]
                b
            ]
        ]

titulo : String -> Html msg
titulo s =
    h1
        ( textStyle "2em" )
        [ text s ]

originalrow : List Int -> Html msg
originalrow r =
    div [ style "overflow" "auto" ]
        [ table
            tableStyle
            [ tr
                [ style "box-shadow" "5px 8px" ]
                ( List.map
                  ((\a -> modBy (List.length r) a)
                      >> String.fromInt
                      >> (\x -> td boldStyle [ text x ]))
                  r
                )
            ]
        ]


entrada : Attribute msg -> Html msg
entrada inp =
    input
        ( fieldStyle
        ++ [ style "margin-top" "50px"
        , style "margin-bottom" "50px"
        , placeholder "4 5 7 1 6 3 8 2 11 0 9 10"
        , inp
        ])
        []


bloquealeatorio : Attribute msg -> Attribute msg -> Html msg
bloquealeatorio longitud aleatorio =
    div
        []
        [ span
            (textStyle "20px")
            [ text "Serie aleatoria de longitud " ]
        , input
            ( squarefieldStyle "1em"
            ++ [ placeholder "12"
            , longitud
            ])
            []
        , button ( littlebuttonStyle ++ [ aleatorio ])
            [ text "OK" ]
        ]

aStyle : Bool -> String -> List (Attribute msg)
aStyle this ref =
  [ href (if this then "" else ref)
  , style "background-color" (if this then bold else suave)
  , style "color" "black"
  , style "text-decoration" "none"
  , style "padding" "2px"
  , style "min-width" "210px"
  , style "display" "inline-block"
  , style "border-width" "0px 3px 0px 3px"
  , style "border-style" "solid"
  , style "border-color" back
  ]
  ++
  textStyle "1.25em"

navbar : Int -> Html msg
navbar this =
  let (one,two,three) =
        (this==1,this==2,this==3)
  in
  div []
  [ nav
    [ style "position" "fixed"
    , style "top" "0"
    , style "width" "100%"
    , style "background-color" back
    ]
    [ a (aStyle one "https://matrices.netlify.com/")
      [ text "Matrices"]
    , a (aStyle two "https://diagramas.netlify.com/")
      [ text "Diagramas"]
    , a (aStyle three "https://modificaciones.netlify.com/")
      [ text "Modificaciones"]
    , div [style "min-width" "15px",style "display" "inline-block"] [text " "]
    ]
    , br [] []
    , br [] []
  ]
