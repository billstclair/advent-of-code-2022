----------------------------------------------------------------
--
-- Day04.elm
-- 2022 Advent of Code
-- https://adventofcode.com/2022/day/3#part2
-- Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Day04 exposing (main)

import Browser
import Html exposing (Attribute, Html, a, div, h2, p, text, textarea)
import Html.Attributes exposing (cols, href, rows, style, target, value)
import Html.Events exposing (onInput)
import Set exposing (Set)



{- Customize below here for each puzzle. -}


{-| Select part 1 or 2 of the puzzle
-}
part : Int
part =
    2


dayStrings =
    { day = "Day 4"
    , aocUrl = "https://adventofcode.com/2022/day/4"
    , githubUrl = "https://github.com/billstclair/advent-of-code-2022/blob/main/Day04.elm"
    }


type alias Range =
    { start : Int
    , end : Int
    }


parseRange : String -> Range
parseRange string =
    case String.split "-" string of
        [ start, end ] ->
            { start = Maybe.withDefault 0 <| String.toInt start
            , end = Maybe.withDefault 0 <| String.toInt end
            }

        _ ->
            Debug.log ("parseRange " ++ string ++ ", bad range") <| Range 0 0


parseLine : String -> ( Range, Range )
parseLine string =
    case String.split "," string of
        [ r1, r2 ] ->
            ( parseRange r1, parseRange r2 )

        _ ->
            Debug.log (string ++ ": Not two elves in line") ( Range 0 0, Range 1 1 )


fullyContains : ( Range, Range ) -> Bool
fullyContains ( range1, range2 ) =
    (range1.start >= range2.start && range1.end <= range2.end)
        || (range2.start >= range1.start && range2.end <= range1.end)


overlaps : ( Range, Range ) -> Bool
overlaps ( range1, range2 ) =
    fullyContains ( range1, range2 )
        || (range1.start >= range2.start && range1.start <= range2.end)
        || (range1.end >= range2.start && range1.end <= range2.end)


predicate : ( Range, Range ) -> Bool
predicate =
    if part == 1 then
        fullyContains

    else
        overlaps


solve : String -> String
solve input =
    let
        folder : ( Range, Range ) -> Int -> Int
        folder ranges sum =
            if predicate ranges then
                sum + 1

            else
                sum
    in
    String.split "\n" input
        |> List.map parseLine
        |> List.foldl folder 0
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
