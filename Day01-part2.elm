----------------------------------------------------------------
--
-- Day01-part2.elm
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
import Html exposing (Attribute, Html, a, div, h2, p, text, textarea)
import Html.Attributes exposing (cols, href, rows, style, target, value)
import Html.Events exposing (onInput)


{-| Customize this for each puzzle.
-}
solve : String -> String
solve input =
    let
        elves =
            String.split "\n\n" input

        byElf =
            List.map (String.split "\n") elves

        itemsPerElf =
            List.map
                (List.map (Maybe.withDefault 0 << String.toInt))
                byElf

        calories =
            List.map (List.foldl (\i res -> i + res) 0) itemsPerElf

        sortedCalories =
            List.sortWith (\x y -> compare y x) calories

        top3 =
            List.take 3 sortedCalories
    in
    List.foldl (\i res -> i + res) 0 top3
        |> String.fromInt


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
                "Advent of Code "
                    ++ special.middleDot
                    ++ " Day 1, Part 2"
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
            , text " : "
            , link "Day 2" "https://adventofcode.com/2022/day/1#part2"
            , br
            , text "GitHub: "
            , let
                url =
                    "https://github.com/billstclair/advent-of-code-2022/blob/main/Day01-part2.elm"
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
