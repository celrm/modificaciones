module Main exposing (main)

import Browser
import State exposing (init, subscriptions, update)
import Types exposing (..)
import View exposing (view)


main =
    Browser.document
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
