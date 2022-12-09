----------------------------------------------------------------
--
-- Day08.elm
-- 2022 Advent of Code
-- https://adventofcode.com/2022/day/8
-- Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Day08 exposing (main)

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
    2


partSuffix =
    if part == 0 then
        ""

    else
        ", part " ++ String.fromInt part


dayStrings =
    { day = "Day 8" ++ partSuffix
    , aocUrl = "https://adventofcode.com/2022/day/8"
    , githubUrl = "https://github.com/billstclair/advent-of-code-2022/blob/main/Day08.elm"
    }


type alias Forest =
    Array (Array Int)


emptyForest : Forest
emptyForest =
    Array.empty


getHeight : Int -> Int -> Forest -> Int
getHeight x y forest =
    case Array.get y forest of
        Nothing ->
            0

        Just array ->
            case Array.get x array of
                Nothing ->
                    0

                Just h ->
                    h


setHeight : Int -> Int -> Int -> Forest -> Forest
setHeight x y height forest =
    let
        array =
            case Array.get y forest of
                Nothing ->
                    Array.empty

                Just a ->
                    a
    in
    Array.set y (Array.set x height array) forest


makeRow : String -> Array Int
makeRow string =
    let
        charToInt char =
            String.fromList [ char ]
                |> String.toInt
                |> Maybe.withDefault 0
    in
    String.toList string
        |> List.map charToInt
        |> Array.fromList


stringToForest : String -> Forest
stringToForest string =
    string
        |> String.split "\n"
        |> List.filter ((/=) Nothing << String.toInt)
        |> List.map makeRow
        |> Array.fromList


aref : Int -> Int -> Forest -> Int
aref x y forest =
    case Array.get y forest of
        Nothing ->
            0

        Just row ->
            case Array.get x row of
                Nothing ->
                    0

                Just h ->
                    h


isVisible : Int -> Int -> Forest -> Bool
isVisible x y forest =
    let
        rowcnt =
            Array.length forest
    in
    if x <= 0 || y <= 0 || y >= rowcnt - 1 then
        True

    else
        case Array.get y forest of
            Nothing ->
                False

            Just row ->
                let
                    colcnt =
                        Array.length row
                in
                if x >= colcnt - 1 then
                    True

                else
                    let
                        height =
                            aref x y forest

                        xloop ox oy maxx =
                            if ox >= maxx then
                                True

                            else if height <= aref ox oy forest then
                                False

                            else
                                xloop (ox + 1) oy maxx

                        yloop ox oy maxy =
                            if oy >= maxy then
                                True

                            else if height <= aref ox oy forest then
                                False

                            else
                                yloop ox (oy + 1) maxy
                    in
                    xloop 0 y x
                        || xloop (x + 1) y colcnt
                        || yloop x 0 y
                        || yloop x (y + 1) rowcnt


countVisibleTrees : Forest -> Int
countVisibleTrees forest =
    let
        rowcnt =
            Array.length forest

        loop y res =
            if y >= rowcnt then
                res

            else
                case Array.get y forest of
                    Nothing ->
                        loop (y + 1) res

                    Just row ->
                        let
                            colcnt =
                                Array.length row

                            inner x ires =
                                if x >= colcnt then
                                    loop (y + 1) ires

                                else if isVisible x y forest then
                                    inner (x + 1) <| ires + 1

                                else
                                    inner (x + 1) ires
                        in
                        inner 0 res
    in
    loop 0 0


part1 : String -> String
part1 input =
    stringToForest input
        |> countVisibleTrees
        |> String.fromInt


scenicScore : Int -> Int -> Forest -> Int
scenicScore x y forest =
    let
        maxrow =
            Array.length forest - 1
    in
    if x <= 0 || y <= 0 || y >= maxrow then
        0

    else
        case Array.get y forest of
            Nothing ->
                0

            Just row ->
                let
                    maxcol =
                        Array.length row - 1
                in
                if x >= maxcol then
                    0

                else
                    let
                        height =
                            aref x y forest

                        leftLoop ox oy res =
                            if ox < 0 then
                                res

                            else if height <= aref ox oy forest then
                                res + 1

                            else
                                leftLoop (ox - 1) oy <| res + 1

                        rightLoop ox oy res =
                            if ox > maxcol then
                                res

                            else if height <= aref ox oy forest then
                                res + 1

                            else
                                rightLoop (ox + 1) oy <| res + 1

                        upLoop ox oy res =
                            if oy < 0 then
                                res

                            else if height <= aref ox oy forest then
                                res + 1

                            else
                                upLoop ox (oy - 1) <| res + 1

                        downLoop ox oy res =
                            if oy > maxrow then
                                res

                            else if height <= aref ox oy forest then
                                res + 1

                            else
                                downLoop ox (oy + 1) <| res + 1
                    in
                    upLoop x (y - 1) 0
                        * leftLoop (x - 1) y 0
                        * downLoop x (y + 1) 0
                        * rightLoop (x + 1) y 0


findBestScenicScore : Forest -> Int
findBestScenicScore forest =
    let
        maxrow =
            Array.length forest - 1

        rowLoop y rres =
            if y > maxrow then
                rres

            else
                case Array.get y forest of
                    Nothing ->
                        rowLoop (y + 1) rres

                    Just row ->
                        let
                            maxcol =
                                Array.length row - 1

                            colLoop x cres =
                                if x > maxcol then
                                    rowLoop (y + 1) cres

                                else
                                    let
                                        score =
                                            scenicScore x y forest
                                    in
                                    if score > cres then
                                        colLoop (x + 1) score

                                    else
                                        colLoop (x + 1) cres
                        in
                        colLoop 0 rres
    in
    rowLoop 0 0


part2 : String -> String
part2 input =
    stringToForest input
        |> findBestScenicScore
        |> String.fromInt


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
