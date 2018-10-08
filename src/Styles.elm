module Styles exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (style)


generalStyle : Attribute msg
generalStyle =
  style
    [ ("text-align", "center")
    , ("background-color", "#F9F9FF")
    , ("overflow", "auto")
    ]


textStyle : String -> Attribute msg
textStyle tam =
  style
    [ ("font-family", "calibri")
    , ("font-size", tam)
    ]


cellStyle : Attribute msg
cellStyle =
  style
    [ ("text-align", "center")
    , ("width", "50px")
    , ("max-width", "50px")
    , ("min-width", "50px")
    , ("border", "1px solid black")
    , ("font-family","calibri")
    , ("background-color", "#EEEEFF")
    ]


boldStyle : Attribute msg
boldStyle =
  style
    [ ("text-align", "center")
    , ("width", "50px")
    , ("max-width", "50px")
    , ("min-width", "50px")
    , ("border", "3px solid black")
    , ("font-weight","bold")
    , ("font-family","calibri")
    , ("background-color", "#BBBBFF")
    , ("box-shadow","5px 7.5px")
    --, ("-webkit-user-select","none")
    --, ("-khtml-user-select","none")
    --, ("-moz-user-select","none")
    --, ("-o-user-select","none")
    --, ("user-select","none")
    ]


tableStyle : Attribute msg
tableStyle =
  style
    [ ("text-align", "center")
    , ("border-collapse", "collapse")
    , ("table-layout", "fixed")
    , ("margin-left", "auto")
    , ("margin-right", "auto")
    , ("margin-top","50px")
    , ("margin-bottom","20px")
    ]


selectStyle : Attribute msg
selectStyle =
  style
    [ ("font-family","calibri")
    , ("font-size", "20px")
    , ("width", "208px")
    , ("padding", "3.5px 10px")
    , ("appearance","none")
    , ("-moz-appearance","none")
    , ("-webkit-appearance", "none")
    , ("background-image","url(http://i62.tinypic.com/15xvbd5.png)")
    , ("background-repeat","no-repeat")
    , ("background-position","93%")
    , ("border", "1px solid #88888")
    , ("border-radius", "20px 20px 20px 20px")
    , ("color", "black")
    , ("overflow","hidden")
    , ("-moz-border-radius", "20px 20px 20px 20px")
    , ("-webkit-border-radius", "20px 2top0px 20px 20px")
    , ("outline","none")
    ]


buttonStyle : String -> Attribute msg
buttonStyle s =
  style
    [ ("font-family","calibri")
    , ("font-size", "20px")
    , ("width", s)
    ]


fileStyle : Attribute msg
fileStyle =
  style
    [ ("font-family","calibri")
    , ("font-size", "20px")
    , ("margin-left", "auto")
    , ("margin-right", "auto")
    , ("padding", "3.5px 10px")
    , ("appearance","none")
    , ("-moz-appearance","none")
    , ("-webkit-appearance", "none")
    , ("background-repeat","no-repeat")
    , ("background-position","95%")
    , ("border", "1px solid #88888")
    , ("border-radius", "20px 20px 20px 20px")
    , ("color", "black")
    , ("overflow","hidden")
    , ("-moz-border-radius", "20px 20px 20px 20px")
    , ("-webkit-border-radius", "20px 20px 20px 20px")
    ]


littlebuttonStyle : Attribute msg
littlebuttonStyle =
  style
    [ ("font-family","calibri")
    , ("font-size", "20px")
    , ("width", "50px")
    , ("padding", "3.5px 10px")
    , ("appearance","none")
    , ("-moz-appearance","none")
    , ("-webkit-appearance", "none")
    , ("background-repeat","no-repeat")
    , ("background-position","95%")
    , ("border", "1px solid #88888")
    , ("border-radius", "20px 20px 20px 20px")
    , ("color", "black")
    , ("overflow","hidden")
    , ("-moz-border-radius", "20px 20px 20px 20px")
    , ("-webkit-border-radius", "20px 20px 20px 20px")
    , ("font-family","calibri")
    ]



fieldStyle : Attribute msg
fieldStyle =
  style
    [ ("width", "650px")
    , ("text-align", "center")
    , ("font-family","calibri")
    , ("font-size", "30px")
    , ("border-radius", "20px 20px 20px 20px")
    ]


squarefieldStyle : String -> Attribute msg
squarefieldStyle s =
  style
    [ ("width", s)
    , ("text-align", "center")
    , ("font-family","calibri")
    , ("font-size", "20px")
    , ("padding", "3.5px 2px")
    ]
