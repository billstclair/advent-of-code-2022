----------------------------------------------------------------
--
-- Day10.elm
-- 2022 Advent of Code
-- https://adventofcode.com/2022/day/10
-- Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Day10 exposing (..)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, code, div, h2, p, pre, text, textarea)
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
    { day = "Day 10" ++ partSuffix
    , aocUrl = "https://adventofcode.com/2022/day/10"
    , githubUrl = "https://github.com/billstclair/advent-of-code-2022/blob/main/Day10.elm"
    }


type alias State =
    { x : Int
    , cycle : Int
    , history : List { cycle : Int, x : Int }
    , delta : Int
    , pixels : String
    }


initialState : State
initialState =
    { x = 1
    , cycle = 1
    , history = []
    , delta = 0
    , pixels = ""
    }


type Instruction
    = Addx Int
    | Noop


addPixel : Int -> Int -> String
addPixel x cycle =
    let
        pixelNo =
            modBy 40 <| cycle - 1

        newLine =
            if pixelNo == 0 && cycle /= 1 then
                "\n"

            else
                ""

        pixel =
            if pixelNo >= x - 1 && pixelNo <= x + 1 then
                "#"

            else
                "."
    in
    newLine ++ pixel


execute : Instruction -> State -> State
execute instruction state =
    let
        newX =
            state.x + state.delta

        pixel =
            addPixel newX state.cycle

        ( ( newCycle, newHistory, newDelta ), pixels ) =
            case instruction of
                Noop ->
                    ( ( state.cycle + 1
                      , [ { x = newX, cycle = state.cycle } ]
                      , 0
                      )
                    , pixel
                    )

                Addx delta ->
                    ( ( state.cycle + 2
                      , [ { x = newX, cycle = state.cycle + 1 }
                        , { x = newX, cycle = state.cycle }
                        ]
                      , delta
                      )
                    , pixel ++ addPixel newX (state.cycle + 1)
                    )
    in
    { state
        | x = newX
        , cycle = newCycle
        , history = List.append newHistory state.history
        , delta = newDelta
        , pixels = state.pixels ++ pixels
    }


parseInstruction : String -> Maybe Instruction
parseInstruction string =
    case String.split " " string of
        [ "noop" ] ->
            Just Noop

        [ "addx", int ] ->
            case String.toInt int of
                Nothing ->
                    Nothing

                Just i ->
                    Just <| Addx i

        _ ->
            Nothing


parseInput : String -> List Instruction
parseInput string =
    String.split "\n" string
        |> List.map parseInstruction
        |> List.filterMap identity


part1 : String -> String
part1 input =
    parseInput input
        |> List.foldl execute initialState
        |> Debug.log "final state"
        |> .history
        |> List.filter
            (\{ cycle } ->
                modBy 40 (cycle - 20) == 0 && cycle <= 220
            )
        |> Debug.log "interesting cycles"
        |> List.map (\{ cycle, x } -> cycle * x)
        |> Debug.log "signal strengths"
        |> List.foldl (+) 0
        |> String.fromInt


part2 : String -> String
part2 input =
    parseInput input
        |> List.foldl execute initialState
        |> Debug.log "final state"
        |> .pixels


solve : String -> String
solve input =
    if part == 1 then
        part1 input

    else
        part2 input


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
        , pre []
            [ text model.output ]
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
