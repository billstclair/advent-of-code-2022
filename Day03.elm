----------------------------------------------------------------
--
-- Day03.elm
-- 2022 Advent of Code
-- https://adventofcode.com/2022/day/3
-- Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Day03 exposing (main)

import Browser
import Html exposing (Attribute, Html, a, div, h2, p, text, textarea)
import Html.Attributes exposing (cols, href, rows, style, target, value)
import Html.Events exposing (onInput)
import Set exposing (Set)



{- Customize below here for each puzzle. -}


dayStrings =
    { day = "Day 3"
    , aocUrl = "https://adventofcode.com/2022/day/3"
    , githubUrl = "https://github.com/billstclair/advent-of-code-2022/blob/main/Day03.elm"
    }


solve : String -> String
solve input =
    let
        rucksacks : List String
        rucksacks =
            String.split "\n" input

        splitRucksack : String -> ( String, String )
        splitRucksack rucksack =
            let
                compartmentLen =
                    String.length rucksack // 2
            in
            ( String.left compartmentLen rucksack
            , String.dropLeft compartmentLen rucksack
            )

        compartments : List ( String, String )
        compartments =
            List.map splitRucksack <| rucksacks

        itemsSet : String -> Set Char
        itemsSet rucksack =
            String.toList rucksack
                |> Set.fromList

        sharedItems : Set Char -> Set Char -> List Char
        sharedItems compartment1 compartment2 =
            Set.intersect compartment1 compartment2
                |> Set.toList

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

        folder : ( String, String ) -> Int -> Int
        folder ( compartment1, compartment2 ) total =
            let
                ( set1, set2 ) =
                    ( itemsSet compartment1, itemsSet compartment2 )

                shared =
                    sharedItems set1 set2

                priorities =
                    List.map itemPriority shared
            in
            total + List.foldl (+) 0 priorities
    in
    List.foldl folder 0 compartments
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
