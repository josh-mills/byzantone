module Tests exposing (..)

import Array
import Byzantine.Degree as Degree exposing (Degree(..))
import Byzantine.Pitch as Pitch
import Byzantine.Scale as Scale exposing (Scale(..))
import Expect
import Test exposing (Test, describe, test)


degreeTests : Test
degreeTests =
    describe "Test Degree logic"
        [ describe "indexOf is correct"
            (Degree.gamut
                |> Array.toList
                |> List.indexedMap
                    (\i degree ->
                        test ("getting " ++ String.fromInt i) <|
                            \_ ->
                                Expect.equal (Just degree)
                                    (Array.get (Degree.indexOf degree) Degree.gamut)
                    )
            )
        , describe "gamut is exhaustive" <|
            [ test "length is correct" <|
                \_ ->
                    Expect.equal
                        (List.length gamutBuilder)
                        (Array.length Degree.gamut)
            , test "contents match" <|
                \_ ->
                    Expect.equal
                        (Array.toList Degree.gamut)
                        gamutBuilder
            ]
        , describe "all pitches have a pitch position" <|
            List.concatMap
                (\scale ->
                    List.map
                        (\degree ->
                            test (Scale.name scale ++ " pitch position for " ++ Degree.toString degree ++ " is not negative") <|
                                \_ ->
                                    Expect.greaterThan -1 (Pitch.pitchPosition scale degree)
                        )
                        gamutBuilder
                )
                Scale.all
        ]


gamutBuilder : List Degree
gamutBuilder =
    let
        next : List Degree -> List Degree
        next degrees =
            case List.head degrees of
                Nothing ->
                    GA :: degrees |> next

                Just GA ->
                    DI :: degrees |> next

                Just DI ->
                    KE :: degrees |> next

                Just KE ->
                    Zo :: degrees |> next

                Just Zo ->
                    Ni :: degrees |> next

                Just Ni ->
                    Pa :: degrees |> next

                Just Pa ->
                    Bou :: degrees |> next

                Just Bou ->
                    Ga :: degrees |> next

                Just Ga ->
                    Di :: degrees |> next

                Just Di ->
                    Ke :: degrees |> next

                Just Ke ->
                    Zo_ :: degrees |> next

                Just Zo_ ->
                    Ni_ :: degrees |> next

                Just Ni_ ->
                    Pa_ :: degrees |> next

                Just Pa_ ->
                    Bou_ :: degrees |> next

                Just Bou_ ->
                    Ga_ :: degrees |> next

                Just Ga_ ->
                    degrees
    in
    next [] |> List.reverse
