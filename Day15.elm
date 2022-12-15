----------------------------------------------------------------
--
-- Day15.elm
-- 2022 Advent of Code
-- https://adventofcode.com/2022/day/15
-- Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Day15 exposing (..)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, code, div, h2, p, pre, span, text, textarea)
import Html.Attributes exposing (cols, href, rows, style, target, value)
import Html.Events exposing (onInput)
import Set exposing (Set)



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
    { day = "Day 15" ++ partSuffix
    , aocUrl = "https://adventofcode.com/2022/day/15"
    , githubUrl = "https://github.com/billstclair/advent-of-code-2022/blob/main/Day15.elm"
    }


type alias Point =
    ( Int, Int )


px : Point -> Int
px =
    Tuple.first


py : Point -> Int
py =
    Tuple.second


makePoint : Int -> Int -> Point
makePoint x y =
    ( x, y )


manhattanDistance : Point -> Point -> Int
manhattanDistance ( x1, y1 ) ( x2, y2 ) =
    abs (x1 - x2) + abs (y1 - y2)


type alias Sensor =
    { at : Point
    , beacon : Point
    }


maybePoint : Maybe Int -> Maybe Int -> Maybe Point
maybePoint mx my =
    case mx of
        Just x ->
            case my of
                Just y ->
                    Just <| makePoint x y

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


addSensor : Sensor -> Set Point -> Set Point
addSensor { at, beacon } points =
    let
        ( atx, aty ) =
            at

        distance =
            manhattanDistance at beacon

        range : Int -> Int -> (Int -> Set Point -> Set Point) -> Set Point -> Set Point
        range from to adjoiner ps =
            let
                loop : Int -> Set Point -> Set Point
                loop mid ps2 =
                    if mid > to then
                        ps2

                    else
                        loop (mid + 1) <| adjoiner mid ps2
            in
            loop from ps

        rangey : Int -> Set Point -> Set Point
        rangey x ps =
            let
                deltay =
                    abs (distance - x)

                addOne : Int -> Set Point -> Set Point
                addOne y ps2 =
                    let
                        p =
                            ( x, y )
                    in
                    if p /= beacon then
                        Set.insert p ps2

                    else
                        ps2
            in
            range (aty - deltay) (aty + deltay) addOne ps
    in
    range (atx - distance) (atx + distance) rangey points


{-| Simplistic, I know, but it works
-}
parseSensor : String -> Maybe Sensor
parseSensor string =
    case log "parseSensor" <| String.split "=" string of
        [ _, sxsp, sysp, bxsp, bys ] ->
            case String.split "," sxsp of
                sxs :: _ ->
                    case String.split ":" sysp of
                        sys :: _ ->
                            case String.split "," bxsp of
                                bxs :: _ ->
                                    let
                                        msp =
                                            maybePoint
                                                (String.toInt sxs)
                                            <|
                                                String.toInt sys

                                        mbp =
                                            maybePoint
                                                (String.toInt bxs)
                                            <|
                                                String.toInt bys
                                    in
                                    case ( msp, mbp ) of
                                        ( Just sp, Just bp ) ->
                                            Just <| Sensor sp bp

                                        _ ->
                                            Nothing

                                _ ->
                                    Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


filter : Point -> Bool
filter ( x, y ) =
    y == 10


part1 : String -> String
part1 input =
    String.split "\n" input
        |> List.filterMap parseSensor
        |> log "sensors"
        |> List.foldl addSensor Set.empty
        |> Set.filter filter
        |> log "filtered"
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


log : String -> x -> x
log s x =
    --x
    Debug.log s x


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
        , span {- pre -} []
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
