module Credits exposing (..)

import Styles exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

credits : Html msg
credits =
  div
  ([ style "margin-top" "100px"
  , style "right" "0px"
  , style "bottom" "0px"
  , style "position" "fixed"
  , style "background-color" back
  ] ++ textStyle "0.75em")
  [ span [] [text "Celia Rubio Madrigal"]
  , span [style "margin-left" "50px"] [text "Email: "]
  , a [ href "mailto:celrubio@ucm.es",style "margin-right" "20px"] [text "celrubio@ucm.es"]
  ]
