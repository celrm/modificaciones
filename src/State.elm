module State exposing (init, subscriptions, update)

import File.Select as Select
import File.Download as Download
import File exposing (File)
import Types exposing (..)
import Functions exposing (modify)
import Task


init : () -> (Model, Cmd Msg)
init _ =
  ( { hover = False
    , scores = []
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
        SelectScores ->
            ( model
            , Select.files ["application/octet-stream"] GotFiles
            )

        GotFiles file files ->
          let newFiles =
                file :: files
              oneCmd f =
                Task.perform (Añadir (File.name f)) (File.toString f)
          in
          ( { model
            | hover = False
            , scores = []
            }
            , Cmd.batch (List.map oneCmd newFiles)
          )

        Añadir name str ->
          let
              newScore =
                  { contents = str
                  , filename = name
                  }
          in
          ( { model
            | scores = newScore :: model.scores
            , hover = False
            }
          , Cmd.none
          )

        DragEnter ->
          ( { model | hover = True }
          , Cmd.none
          )

        DragLeave ->
          ( { model | hover = False }
          , Cmd.none
          )

        Descargar ->
          let newScores =
                List.map (modify model.escala) model.scores
              oneCmd score =
                (Download.string score.filename "text/plain" score.contents)
          in
          ( model
          , Cmd.batch (List.map oneCmd newScores)
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
    Sub.none
