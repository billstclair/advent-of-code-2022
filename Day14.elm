----------------------------------------------------------------
--
-- Day14.elm
-- 2022 Advent of Code
-- https://adventofcode.com/2022/day/14
-- Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Day14 exposing (..)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, code, div, h2, p, pre, span, text, textarea)
import Html.Attributes exposing (cols, href, rows, style, target, value)
import Html.Events exposing (onInput)
import List.Extra as LE
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
    { day = "Day 14" ++ partSuffix
    , aocUrl = "https://adventofcode.com/2022/day/14"
    , githubUrl = "https://github.com/billstclair/advent-of-code-2022/blob/main/Day14.elm"
    }


type alias Point =
    ( Int, Int )


type alias Wall =
    { points : Set Point
    , highY : Int
    }


emptyWall : Wall
emptyWall =
    { points = Set.empty
    , highY = 0
    }


addPoint : Point -> Wall -> Wall
addPoint point wall =
    { wall
        | points = Set.insert point wall.points
        , highY = max wall.highY <| Tuple.second point
    }


toInt : String -> Int
toInt string =
    String.toInt string
        |> Maybe.withDefault 0


parseLine : String -> List Point
parseLine line =
    let
        stringsToPoint : List String -> Point
        stringsToPoint strings =
            case strings of
                [ xs, ys ] ->
                    ( toInt xs, toInt ys )

                _ ->
                    let
                        i =
                            Debug.log "Not a string pair" strings
                    in
                    ( 0, 0 )
    in
    String.split " -> " line
        |> List.map (String.split ",")
        |> List.map stringsToPoint


pointsToWall : List Point -> Wall -> Wall
pointsToWall points wall =
    case points of
        [] ->
            wall

        firstPoint :: rest ->
            let
                loop : Point -> Point -> Wall -> Wall
                loop p1 p2 theWall =
                    let
                        ( x1, y1 ) =
                            p1

                        ( x2, y2 ) =
                            p2
                    in
                    if x1 == x2 then
                        if y1 == y2 then
                            addPoint p2 theWall

                        else
                            let
                                y3 =
                                    if y1 < y2 then
                                        y1 + 1

                                    else
                                        y1 - 1
                            in
                            loop ( x1, y3 ) p2 <| addPoint p1 theWall

                    else if y1 == y2 then
                        let
                            x3 =
                                if x1 < x2 then
                                    x1 + 1

                                else
                                    x1 - 1
                        in
                        loop ( x3, y1 ) p2 <| addPoint p1 theWall

                    else
                        -- Can't happen
                        theWall

                outer : Point -> List Point -> Wall -> Wall
                outer p1 ps theWall =
                    case ps of
                        [] ->
                            theWall

                        p2 :: tail ->
                            outer p2 tail <| loop p1 p2 theWall
            in
            outer firstPoint rest wall


sandStart : Point
sandStart =
    ( 500, 0 )


dropSand : Wall -> Maybe Wall
dropSand wall =
    let
        maxy =
            wall.highY

        loop : Int -> Int -> Wall -> Maybe Wall
        loop x y theWall =
            let
                newy =
                    y + 1

                newp =
                    ( x, newy )
                        |> log "newp"

                points =
                    theWall.points
            in
            if part == 2 && newy > (maxy + 1) then
                Just { theWall | points = Set.insert ( x, y ) points }

            else if part == 1 && newy > maxy then
                Nothing

            else if not <| Set.member newp points then
                loop x newy theWall

            else
                let
                    newx =
                        x - 1
                in
                if not <| Set.member ( newx, newy ) points then
                    loop newx newy theWall

                else
                    let
                        newx2 =
                            x + 1
                    in
                    if not <| Set.member ( newx2, newy ) points then
                        loop newx2 newy theWall

                    else
                        let
                            stopPoint =
                                ( x, y )
                                    |> log "stopPoint"
                        in
                        Just { theWall | points = Set.insert stopPoint points }

        ( startx, starty ) =
            sandStart
    in
    if Set.member sandStart wall.points then
        Nothing

    else
        loop startx starty wall


dropLoop : Wall -> Int
dropLoop wall =
    let
        loop : Int -> Wall -> Int
        loop count theWall =
            case dropSand theWall of
                Nothing ->
                    count

                Just wall2 ->
                    loop (count + 1) wall2
    in
    loop 0 wall


part1 : String -> String
part1 input =
    let
        folder : List Point -> Wall -> Wall
        folder points wall =
            pointsToWall points wall
    in
    String.split "\n" input
        |> List.map parseLine
        |> List.foldl folder emptyWall
        |> log "wall"
        |> dropLoop
        |> String.fromInt


part2 : String -> String
part2 input =
    -- `dropSand` uses `part`.
    part1 input


solve : String -> String
solve input =
    if part == 1 then
        part1 input

    else
        part2 input


log : String -> x -> x
log s x =
    --Debug.log s x
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
