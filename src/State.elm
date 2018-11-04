module State exposing (init, subscriptions, update)

import Ports exposing (..)
import Types exposing (..)


init : () -> (Model, Cmd Msg)
init _ =
  ( { id = "ScoreInputId"
      , mScore = Just example
      , escala =
            { nombre = "Cromática"
            , do = 0
            , dos = 1
            , re = 2
            , res = 3
            , mi = 4
            , fa = 5
            , fas = 6
            , sol = 7
            , sols = 8
            , la = 9
            , las = 10
            , si = 11
            }
      , escalaAnterior =
            { nombre = "Cromática"
            , do = 0
            , dos = 1
            , re = 2
            , res = 3
            , mi = 4
            , fa = 5
            , fas = 6
            , sol = 7
            , sols = 8
            , la = 9
            , las = 10
            , si = 11
            }
      , bloqueado = False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ScoreSelected ->
            ( model
            , fileSelected model.id
            )

        ScoreRead data ->
            let
                newScore =
                    { contents = data.contents
                    , filename = data.filename
                    }
            in
            ( { model | mScore = Just newScore }
            , Cmd.none
            )

        Preescalar nombre zero one two three four five six seven eigth nine ten eleven ->
            ( { model
                | escala =
                    { nombre = nombre
                    , do = zero
                    , dos = one
                    , re = two
                    , res = three
                    , mi = four
                    , fa = five
                    , fas = six
                    , sol = seven
                    , sols = eigth
                    , la = nine
                    , las = ten
                    , si = eleven
                    }
                , bloqueado = True
              }
            , Cmd.none
            )

        Introducir s x ->
            let
                n =
                    x
                        |> String.toInt
                        |> Maybe.withDefault 0

                oldEscala =
                    model.escala

                newEscala =
                    case s of
                        Do ->
                            { oldEscala | do = n }

                        Dos ->
                            { oldEscala | dos = n }

                        Re ->
                            { oldEscala | re = n }

                        Res ->
                            { oldEscala | res = n }

                        Mi ->
                            { oldEscala | mi = n }

                        Fa ->
                            { oldEscala | fa = n }

                        Fas ->
                            { oldEscala | fas = n }

                        Sol ->
                            { oldEscala | sol = n }

                        Sols ->
                            { oldEscala | sols = n }

                        La ->
                            { oldEscala | la = n }

                        Las ->
                            { oldEscala | las = n }

                        Si ->
                            { oldEscala | si = n }
            in
            ( { model
                | escala = newEscala
                , escalaAnterior = newEscala
              }
            , Cmd.none
            )

        CambiarNombreEscala s ->
            let
                oldEscala =
                    model.escala

                newEscala =
                    { oldEscala | nombre = s }
            in
            ( { model
                | escala = newEscala
                , escalaAnterior = newEscala
              }
            , Cmd.none
            )

        VolverAEscalaAnterior ->
            ( { model
                | escala = model.escalaAnterior
                , bloqueado = False
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    fileContentRead ScoreRead
