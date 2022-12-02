----------------------------------------------------------------
--
-- Day02.elm
-- 2022 Advent of Code
-- https://adventofcode.com/2022/day/2
-- Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Day01 exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, div, h2, p, text, textarea)
import Html.Attributes exposing (cols, href, rows, style, target, value)
import Html.Events exposing (onInput)


type Choice
    = Rock
    | Paper
    | Scissors


choiceDict : Dict String Choice
choiceDict =
    Dict.fromList
        [ ( "A", Rock )
        , ( "B", Paper )
        , ( "C", Scissors )
        , ( "X", Rock )
        , ( "Y", Paper )
        , ( "Z", Scissors )
        ]


playToChoice : String -> Choice
playToChoice s =
    Maybe.withDefault Rock <| Dict.get s choiceDict


type Winner
    = HeWins
    | YouWin
    | ItsATie


whoWins : Choice -> Choice -> Winner
whoWins hisChoice yourChoice =
    case hisChoice of
        Rock ->
            case yourChoice of
                Rock ->
                    ItsATie

                Paper ->
                    YouWin

                Scissors ->
                    HeWins

        Paper ->
            case yourChoice of
                Rock ->
                    HeWins

                Paper ->
                    ItsATie

                Scissors ->
                    YouWin

        Scissors ->
            case yourChoice of
                Rock ->
                    YouWin

                Paper ->
                    HeWins

                Scissors ->
                    ItsATie


winnerScore : Winner -> Int
winnerScore winner =
    case winner of
        YouWin ->
            6

        ItsATie ->
            3

        HeWins ->
            0


choiceScore : Choice -> Int
choiceScore choice =
    case choice of
        Rock ->
            1

        Paper ->
            2

        Scissors ->
            3


roundScore : Choice -> Choice -> Int
roundScore hisChoice yourChoice =
    choiceScore yourChoice
        + (winnerScore <| whoWins hisChoice yourChoice)


{-| Customize this for each puzzle.
-}
solve : String -> String
solve input =
    let
        rounds =
            String.split "\n" input

        folder : String -> Int -> Int
        folder plays total =
            case String.split " " plays of
                [ hisPlay, yourPlay ] ->
                    let
                        hisChoice =
                            playToChoice hisPlay

                        yourChoice =
                            playToChoice yourPlay
                    in
                    total + roundScore hisChoice yourChoice

                _ ->
                    total
    in
    List.foldl folder 0 rounds
        |> String.fromInt


dayStrings =
    { day = "Day 2"
    , aocUrl = "https://adventofcode.com/2022/day/2"
    , githubUrl = "https://github.com/billstclair/advent-of-code-2022/blob/main/Day02.elm"
    }


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    { input : String
    , output : String
    }


type Msg
    = Input String


init : Model
init =
    { input = ""
    , output = ""
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input input ->
            { model
                | input = input
                , output = solve input
            }


view : Model -> Html Msg
view model =
    div [ style "margin-left" "3em" ]
        [ h2 []
            [ text <|
                "Advent of Code 2022 "
                    ++ special.middleDot
                    ++ " "
                    ++ dayStrings.day
            ]
        , p [] [ text "Paste the input below. The solution will be computed." ]
        , textarea
            [ rows 40
            , cols 80
            , value model.input
            , onInput Input
            ]
            []
        , p [] [ b "Solution: " ]
        , p [] [ text model.output ]
        , p []
            [ text <| "Copyright " ++ special.copyright ++ " 2022, Bill St. Clair"
            , br
            , link "Advent of Code - 2022" "https://adventofcode.com"
            , text " "
            , text special.middleDot
            , text " "
            , link dayStrings.day dayStrings.aocUrl
            , br
            , text "GitHub: "
            , let
                url =
                    dayStrings.githubUrl
              in
              link url url
            ]
        ]


br : Html msg
br =
    Html.br [] []


b : String -> Html msg
b string =
    Html.b [] [ text string ]


stringFromCode : Int -> String
stringFromCode code =
    String.fromList [ Char.fromCode code ]


special =
    { nbsp = stringFromCode 160 -- \u00A0
    , copyright = stringFromCode 169 -- \u00A9
    , biohazard = stringFromCode 9763 -- \u2623
    , black_star = stringFromCode 10036 -- \u2734
    , hourglass = stringFromCode 8987 -- \u231B
    , hourglass_flowing = stringFromCode 9203 -- \u23F3
    , checkmark = stringFromCode 10003 -- \u2713
    , middleDot = stringFromCode 183 -- \u00B7
    }


link : String -> String -> Html Msg
link label url =
    a
        [ href url
        , blankTarget
        ]
        [ text label ]


blankTarget : Attribute msg
blankTarget =
    target "_blank"
