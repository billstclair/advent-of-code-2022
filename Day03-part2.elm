----------------------------------------------------------------
--
-- Day03-part2.elm
-- 2022 Advent of Code
-- https://adventofcode.com/2022/day/3#part2
-- Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Day03Part2 exposing (main)

import Browser
import Html exposing (Attribute, Html, a, div, h2, p, text, textarea)
import Html.Attributes exposing (cols, href, rows, style, target, value)
import Html.Events exposing (onInput)
import Set exposing (Set)



{- Customize below here for each puzzle. -}


dayStrings =
    { day = "Day 3, part 2"
    , aocUrl = "https://adventofcode.com/2022/day/3#part2"
    , githubUrl = "https://github.com/billstclair/advent-of-code-2022/blob/main/Day03-part2.elm"
    }


solve : String -> String
solve input =
    let
        rucksacks : List String
        rucksacks =
            String.split "\n" input

        groupLoop : List String -> List (List String) -> List (List String)
        groupLoop rucksacksTail res =
            if List.length rucksacksTail < 3 then
                List.reverse res

            else
                let
                    newTail =
                        List.take 3 rucksacksTail
                in
                groupLoop (List.drop 3 rucksacksTail) <|
                    newTail
                        :: res

        groups : List (List String)
        groups =
            groupLoop rucksacks []

        itemsSet : String -> Set Char
        itemsSet rucksack =
            String.toList rucksack
                |> Set.fromList

        itemPriority : Char -> Int
        itemPriority item =
            let
                code =
                    Char.toCode item
            in
            if item >= 'a' && item <= 'z' then
                1 + code - Char.toCode 'a'

            else
                27 + code - Char.toCode 'A'

        folder : List String -> Int -> Int
        folder group total =
            let
                sets =
                    List.map itemsSet group

                item =
                    (case List.head sets of
                        Nothing ->
                            Set.empty

                        Just first ->
                            List.foldl Set.intersect
                                first
                                (List.drop 1 sets)
                    )
                        |> Set.toList
                        |> List.head
                        |> Maybe.withDefault
                            (Char.fromCode <| 27 + Char.toCode 'A')
            in
            total + itemPriority item
    in
    List.foldl folder 0 groups
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
