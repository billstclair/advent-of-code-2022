----------------------------------------------------------------
--
-- Day12.elm
-- 2022 Advent of Code
-- https://adventofcode.com/2022/day/11
-- Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Day12 exposing (..)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, code, div, h2, p, pre, text, textarea)
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
    { day = "Day 12" ++ partSuffix
    , aocUrl = "https://adventofcode.com/2022/day/12"
    , githubUrl = "https://github.com/billstclair/advent-of-code-2022/blob/main/Day12.elm"
    }


type alias Row =
    Dict Int Int


type alias Board =
    Dict Int Row


type alias Pair =
    ( Int, Int )


type alias Visited =
    Set Pair


get : Pair -> Board -> Int
get ( row, col ) board =
    case Dict.get row board of
        Nothing ->
            0

        Just r ->
            case Dict.get col r of
                Nothing ->
                    0

                Just v ->
                    v


aChar : Char
aChar =
    'a'


aCode : Int
aCode =
    Char.toCode aChar


startChar : Char
startChar =
    'S'


startCode : Int
startCode =
    Char.toCode startChar


goalCode : Int
goalCode =
    Char.toCode 'E'


yCode : Int
yCode =
    Char.toCode 'y'


type Classification
    = IsGoal
    | AlreadyVisited
    | TooHigh
    | NewPair


tryOne : Int -> Pair -> Visited -> Board -> Classification
tryOne val pair visited board =
    if Set.member pair visited then
        AlreadyVisited

    else
        let
            pairVal =
                log "      pairVal" <|
                    get pair board
        in
        if pairVal == goalCode then
            if val >= yCode then
                IsGoal

            else
                TooHigh

        else if val /= startCode && val < pairVal - 1 then
            TooHigh

        else
            NewPair


tryNeighbors : Int -> Pair -> Visited -> Board -> ( Bool, Visited, Visited )
tryNeighbors val ( row, col ) visited board =
    let
        v =
            log "val" val

        neighbors =
            [ ( row - 1, col )
            , ( row + 1, col )
            , ( row, col - 1 )
            , ( row, col + 1 )
            ]
                |> List.filter (\pair -> get pair board /= 0)
                |> log "neighbors"

        loop : List Pair -> Visited -> Visited -> ( Bool, Set Pair, Visited )
        loop ns res vis =
            let
                vs =
                    log "vis" vis
            in
            case ns of
                [] ->
                    ( False, res, vis )

                neighbor :: rest ->
                    let
                        n =
                            log "  neighbor" neighbor

                        classification =
                            log "  tryOne" <|
                                tryOne val neighbor vis board
                    in
                    case classification of
                        IsGoal ->
                            ( True, Set.union (Set.fromList rest) res, vis )

                        AlreadyVisited ->
                            loop rest res vis

                        TooHigh ->
                            loop rest res vis

                        NewPair ->
                            loop rest
                                (Set.insert neighbor res)
                            <|
                                Set.insert neighbor visited
    in
    loop neighbors Set.empty visited


findGoal : Pair -> Board -> Maybe Int
findGoal start board =
    let
        loop : Int -> List Pair -> Visited -> Visited -> Maybe Int
        loop pathLength pairsTail neighbors visited =
            case pairsTail of
                [] ->
                    if neighbors == Set.empty then
                        Nothing

                    else
                        loop (pathLength + 1)
                            (log ("loop " ++ (String.fromInt <| pathLength + 1))
                                (Set.toList neighbors)
                            )
                            Set.empty
                            visited

                pair :: rest ->
                    let
                        p =
                            log "  pair" pair

                        v =
                            log "  visited" visited

                        ( found, newNeighbors, newVisited ) =
                            log "tryNeighbors" <|
                                tryNeighbors (get pair board) pair visited board
                    in
                    if found then
                        Just pathLength

                    else
                        loop pathLength
                            rest
                            (Set.union newNeighbors neighbors)
                            newVisited
    in
    loop 1 [ start ] Set.empty <| Set.fromList [ start ]


findStart : Board -> Maybe Pair
findStart board =
    let
        findInRow : ( Int, Row ) -> Maybe Pair
        findInRow ( row, r ) =
            case LE.find (\( _, v ) -> v == startCode) <| Dict.toList r of
                Nothing ->
                    Nothing

                Just ( col, _ ) ->
                    Just ( row, col )

        findLoop : List ( Int, Row ) -> Maybe Pair
        findLoop rowList =
            case rowList of
                [] ->
                    Nothing

                pair :: rest ->
                    case findInRow pair of
                        Nothing ->
                            findLoop rest

                        res ->
                            res
    in
    Dict.toList board
        |> findLoop


parseLine : (Char -> Char) -> String -> Row
parseLine mapper line =
    String.toList line
        |> List.map mapper
        |> List.indexedMap (\i c -> ( i, Char.toCode c ))
        |> Dict.fromList


parseBoard : (Char -> Char) -> String -> Board
parseBoard mapper string =
    String.split "\n" string
        |> LE.filterNot ((==) "")
        |> List.indexedMap (\i l -> ( i, parseLine mapper l ))
        |> Dict.fromList


part1 : String -> String
part1 input =
    let
        board =
            log "board" <|
                parseBoard identity input
    in
    case findStart board of
        Nothing ->
            "Can't find start: 'S'"

        Just start ->
            case findGoal start board of
                Nothing ->
                    "Can't find end: 'E' = " ++ String.fromInt goalCode

                Just pathLength ->
                    String.fromInt pathLength


startToA : Char -> Char
startToA char =
    if char == startChar then
        aChar

    else
        char


part2 : String -> String
part2 input =
    let
        board =
            log "board" <|
                parseBoard startToA input

        rowLoop : List ( Int, Row ) -> Int -> Int
        rowLoop rows minLength =
            case rows of
                [] ->
                    minLength

                ( rownum, row ) :: rest ->
                    rowLoop rest <| loop rownum (Dict.toList row) minLength

        loop : Int -> List Pair -> Int -> Int
        loop rownum pairs minLength =
            case pairs of
                [] ->
                    minLength

                ( colnum, val ) :: rest ->
                    if val /= aCode then
                        loop rownum rest minLength

                    else
                        case findGoal ( rownum, colnum ) board of
                            Nothing ->
                                loop rownum rest minLength

                            Just pathLength ->
                                loop rownum rest <| min minLength pathLength
    in
    rowLoop (Dict.toList board) 100000
        |> String.fromInt


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
