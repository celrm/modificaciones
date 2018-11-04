module Types exposing (Escala, Model, Msg(..), Nota(..), Score, example)

import Ports exposing (ScorePortData, fileSelected, fileContentRead)


type Nota
    = Do
    | Dos
    | Re
    | Res
    | Mi
    | Fa
    | Fas
    | Sol
    | Sols
    | La
    | Las
    | Si


type Msg
    = ScoreSelected
    | ScoreRead ScorePortData
    | Introducir Nota String
    | Preescalar String Int Int Int Int Int Int Int Int Int Int Int Int
    | CambiarNombreEscala String
    | VolverAEscalaAnterior


type alias Score =
    { contents : String
    , filename : String
    }


type alias Escala =
    { nombre : String
    , do : Int
    , dos : Int
    , re : Int
    , res : Int
    , mi : Int
    , fa : Int
    , fas : Int
    , sol : Int
    , sols : Int
    , la : Int
    , las : Int
    , si : Int
    }


type alias Model =
    { id : String
    , mScore : Maybe Score
    , escala : Escala
    , escalaAnterior : Escala
    , bloqueado : Bool
    }


example : Score
example =
    { contents = ""
    , filename = "nothing.mscx"
    }
