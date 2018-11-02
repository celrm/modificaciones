module Main exposing (..)

import Html exposing (..)

import Types exposing (..)
import State exposing (init,update,subscriptions)
import View exposing (view)


main : Program Never Model Msg
main =
  program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }
{-
-escalas aleatorias
-más escalas conocidas
-las dos cosas a la vez
-random, randomchromatic(perm),
diatonic 2212221,whole tone 222222,
pentatonic 22323,octotonic 21212121

-}
