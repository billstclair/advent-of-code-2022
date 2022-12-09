----------------------------------------------------------------
--
-- Day09.elm
-- 2022 Advent of Code
-- https://adventofcode.com/2022/day/8
-- Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Day09 exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, div, h2, p, text, textarea)
import Html.Attributes exposing (cols, href, rows, style, target, value)
import Html.Events exposing (onInput)
import Set exposing (Set)
import String.Extra as SE



{- Customize below here for each puzzle. -}


part : Int
part =
    1


partSuffix =
    if part == 0 then
        ""

    else
        ", part " ++ String.fromInt part


dayStrings =
    { day = "Day 9" ++ partSuffix
    , aocUrl = "https://adventofcode.com/2022/day/9"
    , githubUrl = "https://github.com/billstclair/advent-of-code-2022/blob/main/Day09.elm"
    }


type alias Point =
    { x : Int
    , y : Int
    }


pointToTuple : Point -> ( Int, Int )
pointToTuple { x, y } =
    ( x, y )


tupleToPoint : ( Int, Int ) -> Point
tupleToPoint ( x, y ) =
    { x = x
    , y = y
    }


addPoints : Point -> Point -> Point
addPoints { x, y } p2 =
    { x = x + p2.x
    , y = y + p2.y
    }


arePointsAdjacent : Point -> Point -> Bool
arePointsAdjacent { x, y } p2 =
    (abs (x - p2.x) <= 1) && (abs (y - p2.y) <= 1)


moveTailNearHead : Point -> Point -> Point
moveTailNearHead head tail =
    if arePointsAdjacent head tail then
        tail

    else
        let
            moveTowards h t =
                if h < t then
                    t - 1

                else if h > t then
                    t + 1

                else
                    t
        in
        { x = moveTowards head.x tail.x
        , y = moveTowards head.y tail.y
        }


type alias Bridge =
    { head : Point
    , tail : Point
    , tailVisits : Set ( Int, Int )
    }


emptyBridge : Bridge
emptyBridge =
    { head = Point 0 0
    , tail = Point 0 0
    , tailVisits = Set.empty
    }


moveHead : Point -> Bridge -> Bridge
moveHead delta bridge =
    let
        head =
            addPoints bridge.head delta

        tail =
            moveTailNearHead head bridge.tail
    in
    { bridge
        | head = head
        , tail = tail
        , tailVisits = Set.insert (pointToTuple tail) bridge.tailVisits
    }


type Move
    = Right Int
    | Left Int
    | Up Int
    | Down Int


doMove : Move -> Bridge -> Bridge
doMove move bridge =
    let
        ( delta, count ) =
            case move of
                Right c ->
                    ( Point 1 0, c )

                Left c ->
                    ( Point -1 0, c )

                Up c ->
                    ( Point 0 1, c )

                Down c ->
                    ( Point 0 -1, c )

        loop : Int -> Bridge -> Bridge
        loop cnt brdg =
            if cnt <= 0 then
                brdg

            else
                loop (cnt - 1) <| moveHead delta brdg
    in
    loop count bridge


parseMove : String -> Move
parseMove string =
    case String.split " " string of
        [ dir, count ] ->
            case String.toInt count of
                Just c ->
                    case dir of
                        "R" ->
                            Right c

                        "L" ->
                            Left c

                        "D" ->
                            Down c

                        "U" ->
                            Up c

                        _ ->
                            let
                                foo =
                                    Debug.log "Bad dir in" string
                            in
                            Right 0

                Nothing ->
                    let
                        foo =
                            Debug.log "Bad count in" string
                    in
                    Right 0

        _ ->
            let
                foo =
                    Debug.log "Malformed move" string
            in
            Right 0


parseMoves : String -> List Move
parseMoves string =
    String.split "\n" string
        |> List.map parseMove


doMoves : Bridge -> List Move -> Bridge
doMoves bridge moves =
    List.foldl doMove bridge moves


part1 : String -> String
part1 input =
    parseMoves input
        |> doMoves emptyBridge
        |> .tailVisits
        |> Set.size
        |> String.fromInt


part2 : String -> String
part2 input =
    input


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
