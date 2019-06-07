module Functions exposing (modify)

import Html exposing (Html, td, text, tr)
import Regex
import Types exposing (..)


getDifference : Escala -> Int -> Int
getDifference escala k =
    let
        arr =
            if k == 0 then
                escala.do

            else if k == 1 then
                escala.dos

            else if k == 2 then
                escala.re

            else if k == 3 then
                escala.res

            else if k == 4 then
                escala.mi

            else if k == 5 then
                escala.fa

            else if k == 6 then
                escala.fas

            else if k == 7 then
                escala.sol

            else if k == 8 then
                escala.sols

            else if k == 9 then
                escala.la

            else if k == 10 then
                escala.las

            else
                escala.si
    in
    arr - k


tpcCreciente : Int -> Int
tpcCreciente a =
    modBy 12 (a + 7) + 12


tpcDecreciente : Int -> Int
tpcDecreciente a =
    modBy 12 (a - 7) + 12


tpcChange : Int -> Int -> Int
tpcChange a b =
    case compare b 0 of
        EQ ->
            a

        GT ->
            tpcChange (tpcCreciente a) (b - 1)

        LT ->
            tpcChange (tpcDecreciente a) (b + 1)


tpc2note : Int -> Int
tpc2note k =
    if k == 2 then
        0

    else if k == 9 then
        1

    else if k == 4 then
        2

    else if k == 11 then
        3

    else if k == 6 then
        4

    else if k == 1 then
        5

    else if k == 8 then
        6

    else if k == 3 then
        7

    else if k == 10 then
        8

    else if k == 5 then
        9

    else if k == 0 then
        10

    else
        11


detpc : Escala -> (String -> String)
detpc model =
    Regex.replace
        (Maybe.withDefault Regex.never (Regex.fromString "<tpc>[0-9]+</tpc>"))
        (\{ match } ->
            "<tpc>"
                ++ (match
                        |> String.slice 5 -6
                        |> String.toInt
                        |> Maybe.withDefault 21
                        |> (\n ->
                                n
                                    |> (\b a -> (\dividend modulus -> modBy modulus dividend) a b) 12
                                    |> tpc2note
                                    |> getDifference model
                                    |> tpcChange n
                           )
                        |> String.fromInt
                   )
                ++ "</tpc>"
        )


depitch : Escala -> (String -> String)
depitch model =
    Regex.replace
        (Maybe.withDefault Regex.never (Regex.fromString "<pitch>[0-9]+</pitch>"))
        (\{ match } ->
            "<pitch>"
                ++ (match
                        |> String.slice 7 -8
                        |> String.toInt
                        |> Maybe.withDefault 21
                        |> (\n ->
                                n
                                    |> (\b a -> (\dividend modulus -> modBy modulus dividend) a b) 12
                                    |> getDifference model
                                    |> (+) n
                           )
                        |> String.fromInt
                   )
                ++ "</pitch>"
        )

modify : Escala -> Score -> Score
modify escala score =
    let
        finducida =
          escala
    in
    { contents =
        score.contents
            |> depitch finducida
            |> detpc finducida
    , filename =
        String.dropRight 5 score.filename
            ++ " - "
            ++ escala.nombre
            ++ ".mscx"
    }
