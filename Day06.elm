----------------------------------------------------------------
--
-- Day06.elm
-- 2022 Advent of Code
-- https://adventofcode.com/2022/day/3#part2
-- Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Day06 exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, div, h2, p, text, textarea)
import Html.Attributes exposing (cols, href, rows, style, target, value)
import Html.Events exposing (onInput)
import Set exposing (Set)



{- Customize below here for each puzzle. -}


part : Int
part =
    2


partSuffix =
    if part == 0 then
        ""

    else
        ", part " ++ String.fromInt part


dayStrings =
    { day = "Day 6" ++ partSuffix
    , aocUrl = "https://adventofcode.com/2022/day/6"
    , githubUrl = "https://github.com/billstclair/advent-of-code-2022/blob/main/Day06.elm"
    }


distinctChars : Int
distinctChars =
    if part == 1 then
        4

    else
        14


solve : String -> String
solve input =
    let
        loop : Int -> String -> Int
        loop i s =
            if String.length s < distinctChars then
                0

            else
                let
                    left =
                        String.left distinctChars s |> String.toList

                    set =
                        Set.fromList left
                in
                if Set.size set == distinctChars then
                    i + distinctChars

                else
                    loop (i + 1) <| String.dropLeft 1 s
    in
    loop 0 input
        |> String.fromInt


{-| Fixed code follows. Customize above here for each puzzle.
-}
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
            , link "MIT LICENCE"
                "https://github.com/billstclair/advent-of-code-2022/blob/master/LICENSE"
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
