----------------------------------------------------------------
--
-- Day05.elm
-- 2022 Advent of Code
-- https://adventofcode.com/2022/day/3#part2
-- Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Day05 exposing
    ( Stacks
    , addToStacks
    , lineToRearrangment
    , main
    , parseStackLine
    , parseStacks
    )

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


dayStrings =
    { day = "Day 5" ++ ", part " ++ String.fromInt part
    , aocUrl = "https://adventofcode.com/2022/day/5"
    , githubUrl = "https://github.com/billstclair/advent-of-code-2022/blob/main/Day05.elm"
    }


type alias Rearrangement =
    { move : Int
    , from : Int
    , to : Int
    }


pop : List String -> ( String, List String )
pop list =
    ( Maybe.withDefault "" <| List.head list
    , List.drop 1 list
    )


lineToRearrangment : String -> Rearrangement
lineToRearrangment string =
    let
        substrs =
            String.split " " string

        -- "move"
        ( movestr, tail1 ) =
            pop substrs

        ( move, tail2 ) =
            pop tail1

        -- "from"
        ( fromstr, tail3 ) =
            pop tail2

        ( from, tail4 ) =
            pop tail3

        -- "to"
        ( tostr, tail5 ) =
            pop tail4

        ( to, _ ) =
            pop tail5

        toStr str =
            Maybe.withDefault 0 <| String.toInt str
    in
    if movestr /= "move" || fromstr /= "from" || tostr /= "to" then
        { move = 0
        , from = 1
        , to = 1
        }

    else
        { move = toStr move
        , from = toStr from
        , to = toStr to
        }


car : List Char -> Char
car list =
    Maybe.withDefault ' ' <| List.head list


parseStackLine : String -> Maybe (List String)
parseStackLine string =
    let
        chars =
            String.toList string
    in
    if car chars /= '[' && car (List.drop 1 chars) /= ' ' then
        Nothing

    else
        let
            loop : List Char -> List Char -> List Char
            loop tail res =
                if List.length tail < 3 then
                    List.reverse res

                else
                    let
                        elt =
                            if car tail == '[' then
                                car <| List.drop 1 tail

                            else
                                ' '
                    in
                    loop (List.drop 4 tail) <| elt :: res
        in
        loop chars [] |> List.map String.fromChar |> Just


type alias Stacks =
    Dict Int (List String)


addToStacks : List String -> Stacks -> Stacks
addToStacks crates stacks =
    let
        loop : Int -> List String -> Stacks -> Stacks
        loop index cratesTail newStacks =
            case cratesTail of
                [] ->
                    newStacks

                crate :: tail ->
                    if crate == " " then
                        loop (index + 1) tail newStacks

                    else
                        let
                            stack =
                                Maybe.withDefault [] <| Dict.get index newStacks
                        in
                        loop (index + 1)
                            tail
                            (Dict.insert index
                                (List.append stack [ crate ])
                                newStacks
                            )
    in
    loop 1 crates stacks


parseStacks : List String -> ( Stacks, List String )
parseStacks lines =
    let
        loop : List String -> Stacks -> ( Stacks, List String )
        loop linesTail stacks =
            case linesTail of
                [] ->
                    ( stacks, [] )

                line :: rest ->
                    case parseStackLine line of
                        Nothing ->
                            ( stacks, List.drop 1 rest )

                        Just crates ->
                            loop rest <| addToStacks crates stacks
    in
    loop lines Dict.empty


parseInput : String -> ( Stacks, List Rearrangement )
parseInput string =
    let
        lines =
            String.split "\n" string

        ( stacks, linesTail ) =
            parseStacks lines

        rearrangements =
            List.map lineToRearrangment linesTail
    in
    ( stacks, rearrangements )


rearrange : Rearrangement -> Stacks -> Stacks
rearrange { move, from, to } stacks =
    let
        fromCrates =
            Maybe.withDefault [] <| Dict.get from stacks

        movedCrates =
            List.take move fromCrates
                |> (if part == 1 then
                        List.reverse

                    else
                        identity
                   )

        toCrates =
            Maybe.withDefault [] <| Dict.get to stacks
    in
    Dict.insert to (List.concat [ movedCrates, toCrates ]) stacks
        |> Dict.insert from (List.drop move fromCrates)


topsOfStacks : Stacks -> String
topsOfStacks stacks =
    Dict.toList stacks
        |> List.map Tuple.second
        |> List.map (Maybe.withDefault "" << List.head)
        |> String.join ""


solve : String -> String
solve input =
    let
        ( stacks, rearrangements ) =
            parseInput input

        folder : Rearrangement -> Stacks -> Stacks
        folder rearrangement res =
            rearrange rearrangement res
    in
    List.foldl folder stacks rearrangements
        |> topsOfStacks


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
