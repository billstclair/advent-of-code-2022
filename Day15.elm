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


type alias Range =
    List ( Int, Int )


emptyRange : Range
emptyRange =
    []


insertIntoRange : ( Int, Int ) -> Range -> Range
insertIntoRange interval range =
    let
        ( min, max ) =
            interval

        insert : Range -> Range -> Range
        insert r res =
            case r of
                [] ->
                    List.reverse <| interval :: res

                int :: rest ->
                    let
                        ( start, end ) =
                            int
                    in
                    if min >= start then
                        if min <= end then
                            List.append (List.reverse res) <| finishInsert r []

                        else
                            insert rest (int :: res)

                    else if max < start then
                        List.append (int :: res) r

                    else if max <= end then
                        List.append (( min, end ) :: res) rest

                    else
                        List.append (List.reverse res) <|
                            finishInsert (( min, end ) :: rest) []

        finishInsert : Range -> Range -> Range
        finishInsert r res =
            case r of
                [] ->
                    -- can't happen
                    List.reverse res

                int :: rest ->
                    let
                        ( start, end ) =
                            int
                    in
                    if max <= end then
                        List.append (List.reverse res) r

                    else
                        case rest of
                            [] ->
                                List.reverse <| ( start, max ) :: res

                            _ ->
                                finishInsert rest <| int :: res

        removeOverlaps : Range -> Range -> Range
        removeOverlaps res r =
            case r of
                ( s1, e1 ) :: ( s2, e2 ) :: rest ->
                    if e1 >= s2 then
                        removeOverlaps res (( s1, e2 ) :: rest)

                    else
                        removeOverlaps (( s1, e1 ) :: res) <| ( s2, e2 ) :: rest

                _ ->
                    List.append (List.reverse res) r
    in
    insert range []
        |> removeOverlaps []


intervalCount : ( Int, Int ) -> Int
intervalCount ( start, end ) =
    1 + end - start


rangeCount : Range -> Int
rangeCount range =
    List.foldl (\interval sum -> intervalCount interval + sum) 0 range


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


type alias Map =
    Dict Int Range


emptyMap : Map
emptyMap =
    Dict.empty


addSensor : Sensor -> Map -> Map
addSensor sensor map =
    let
        { at, beacon } =
            sensor
                |> log "addSensor sensor"

        ( atx, aty ) =
            at

        ( beaconx, beacony ) =
            beacon

        distance =
            manhattanDistance at beacon

        range : Int -> Int -> (Int -> Map -> Map) -> Map -> Map
        range from to adjoiner mp =
            let
                loop : Int -> Map -> Map
                loop mid m2 =
                    if mid > to then
                        m2

                    else
                        let
                            midp1 =
                                mid + 1

                            m3 =
                                adjoiner mid m2
                        in
                        loop midp1 m3
            in
            loop from mp

        rangex : Int -> Map -> Map
        rangex y mp =
            let
                dolog =
                    y == theY

                deltax =
                    distance - abs (y - aty)

                rng =
                    case Dict.get y mp of
                        Nothing ->
                            emptyRange

                        Just rng2 ->
                            rng2

                newRange =
                    let
                        from =
                            atx - deltax

                        to =
                            atx + deltax

                        trash =
                            maybeLog dolog "  rangex" ( from, to )
                    in
                    if from > beaconx || to < beaconx then
                        insertIntoRange ( from, to ) rng
                            |> maybeLog dolog "    insertIntoRange"

                    else
                        let
                            from1 =
                                if from == beacony then
                                    from + 1

                                else
                                    from

                            to1 =
                                beacony - 1

                            from2 =
                                beacony + 1

                            to2 =
                                if to == beacony then
                                    to - 1

                                else
                                    to

                            rng2 =
                                if from1 <= to1 then
                                    let
                                        t2 =
                                            maybeLog dolog "    i1" ( from1, to1 )
                                    in
                                    insertIntoRange ( from1, to1 ) rng
                                        |> maybeLog dolog "      insertIntoRange"

                                else
                                    rng
                        in
                        if from2 <= to2 then
                            let
                                t3 =
                                    maybeLog dolog "    i2" ( from2, to2 )
                            in
                            insertIntoRange ( from2, to2 ) rng2
                                |> maybeLog dolog "      insertIntoRange"

                        else
                            rng2
            in
            Dict.insert y newRange mp
    in
    range (aty - distance) (aty + distance) rangex map


{-| Simplistic, I know, but it works
-}
parseSensor : String -> Maybe Sensor
parseSensor string =
    case String.split "=" string of
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


exampleY : Int
exampleY =
    10


realY : Int
realY =
    2000000


theY : Int
theY =
    --exampleY
    realY


filter : Point -> Bool
filter ( x, y ) =
    y == theY


part1 : String -> String
part1 input =
    String.split "\n" input
        |> List.filterMap parseSensor
        |> List.foldl addSensor emptyMap
        |> Dict.get theY
        |> Maybe.withDefault emptyRange
        |> log "filtered"
        |> rangeCount
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


maybeLog : Bool -> String -> x -> x
maybeLog bool s x =
    if bool then
        log s x

    else
        x


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
