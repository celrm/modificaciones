module Styles exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (style)


generalStyle : List (Attribute msg)
generalStyle =
  [ style "text-align" "center"
  , style "background-color" "#F9F9FF"
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
    , style "background-color" "#EEEEFF"
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
    , style "background-color" "#BBBBFF"
    , style "box-shadow" "5px 7.5px"
    --, style "-webkit-user-select" "none"
    --, style "-khtml-user-select" "none"
    --, style "-moz-user-select" "none"
    --, style "-o-user-select" "none"
    --, style "user-select" "none"
    ]


tableStyle : List (Attribute msg)
tableStyle =
    [ style "text-align" "center"
    , style "border-collapse" "collapse"
    , style "table-layout" "fixed"
    , style "margin-left" "auto"
    , style "margin-right" "auto"
    , style "margin-top" "50px"
    , style "margin-bottom" "20px"
    ]


selectStyle : List (Attribute msg)
selectStyle =
    [ style "font-family" "calibri"
    , style "font-size" "20px"
    , style "width" "208px"
    , style "padding" "3.5px 10px"
    , style "appearance" "none"
    , style "-moz-appearance" "none"
    , style "-webkit-appearance" "none"
    , style "background-image" "urlstyle http://i62.tinypic.com/15xvbd5.png"
    , style "background-repeat" "no-repeat"
    , style "background-position" "93%"
    , style "border" "1px solid #88888"
    , style "border-radius" "20px 20px 20px 20px"
    , style "color" "black"
    , style "overflow" "hidden"
    , style "-moz-border-radius" "20px 20px 20px 20px"
    , style "-webkit-border-radius" "20px 2top0px 20px 20px"
    , style "outline" "none"
    ]


buttonStyle : String -> List (Attribute msg)
buttonStyle s =
    [ style "font-family" "calibri"
    , style "font-size" "20px"
    , style "width" s
    ]


fileStyle : List (Attribute msg)
fileStyle =
    [ style "font-family" "calibri"
    , style "font-size" "20px"
    , style "margin-left" "auto"
    , style "margin-right" "auto"
    , style "padding" "3.5px 10px"
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
    [ style "font-family" "calibri"
    , style "font-size" "20px"
    , style "width" "50px"
    , style "padding" "3.5px 10px"
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
    , style "font-family" "calibri"
    ]



fieldStyle : List (Attribute msg)
fieldStyle =
    [ style "width" "650px"
    , style "text-align" "center"
    , style "font-family" "calibri"
    , style "font-size" "30px"
    , style "border-radius" "20px 20px 20px 20px"
    ]


squarefieldStyle : String -> List (Attribute msg)
squarefieldStyle s =
    [ style "width" s
    , style "text-align" "center"
    , style "font-family" "calibri"
    , style "font-size" "20px"
    , style "padding" "3.5px 2px"
    ]
