----------------------------------------------------------------
--
-- Day11.elm
-- 2022 Advent of Code
-- https://adventofcode.com/2022/day/11
-- Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Day11 exposing (..)

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
    1


partSuffix =
    if part == 0 then
        ""

    else
        ", part " ++ String.fromInt part


dayStrings =
    { day = "Day 11" ++ partSuffix
    , aocUrl = "https://adventofcode.com/2022/day/11"
    , githubUrl = "https://github.com/billstclair/advent-of-code-2022/blob/main/Day11.elm"
    }


type Operator
    = Add
    | Multiply


type Value
    = Old
    | Integer Int


type alias Operation =
    { operator : Operator
    , leftSide : Value
    , rightSide : Value
    }


defaultOperation : Operation
defaultOperation =
    { operator = Add
    , leftSide = Integer 0
    , rightSide = Integer 0
    }


parseOperation : String -> Operation
parseOperation string =
    case String.split " " string of
        [ _, operator, rightSide ] ->
            Operation
                (if operator == "*" then
                    Multiply

                 else
                    Add
                )
                Old
                (if rightSide == "old" then
                    Old

                 else
                    Integer <| Maybe.withDefault 0 <| String.toInt rightSide
                )

        _ ->
            defaultOperation


sideValue : Int -> Value -> Int
sideValue int value =
    case value of
        Old ->
            int

        Integer v ->
            v


operate : Operator -> Int -> Int -> Int
operate operator x y =
    case operator of
        Add ->
            x + y

        Multiply ->
            x * y


doOperation : Operation -> Int -> Int
doOperation { operator, leftSide, rightSide } int =
    operate operator (sideValue int leftSide) (sideValue int rightSide)


type alias Monkey =
    { number : Int
    , items : List Int
    , operation : Operation
    , divisibleBy : Int
    , ifTrue : Int
    , ifFalse : Int
    }


lastInt : Int -> String -> Int
lastInt default string =
    case String.split " " string |> List.reverse of
        num :: _ ->
            Maybe.withDefault default <| String.toInt num

        _ ->
            default


parseMonkey : String -> Maybe Monkey
parseMonkey string =
    let
        nextNumber n =
            if n == 0 then
                1

            else
                0
    in
    case String.split "\n" string of
        [ number, items, operation, test, ifTrue, ifFalse ] ->
            let
                n =
                    case String.split " " number of
                        [ _, ncolon ] ->
                            String.dropRight 1 ncolon
                                |> String.toInt
                                |> Maybe.withDefault 0

                        _ ->
                            0
            in
            Just <|
                Monkey
                    n
                    (case String.split ":" items of
                        [ _, numbers ] ->
                            String.split "," numbers
                                |> List.map String.trim
                                |> List.map (Maybe.withDefault 0 << String.toInt)

                        _ ->
                            []
                    )
                    (case String.split "=" operation of
                        [ _, op ] ->
                            parseOperation <| String.trim op

                        _ ->
                            defaultOperation
                    )
                    (lastInt 1 test)
                    (lastInt (nextNumber n) ifTrue)
                    (lastInt (nextNumber n) ifFalse)

        _ ->
            Nothing


parseMonkeys : String -> List Monkey
parseMonkeys string =
    String.split "\n\n" string
        |> List.filterMap parseMonkey


doTurn : Monkey -> ( Monkey, Int, Int )
doTurn monkey =
    case monkey.items of
        [] ->
            -- Never happens
            ( monkey, -1, 0 )

        item :: rest ->
            let
                i =
                    log "  inspect" item

                level =
                    log "    level" <|
                        doOperation monkey.operation i

                level2 =
                    log "    level2" <|
                        level
                            // 3

                throwTo =
                    if modBy monkey.divisibleBy level2 == 0 then
                        log "    divisible, throw to"
                            monkey.ifTrue

                    else
                        log "    not divisible, throw to"
                            monkey.ifFalse
            in
            ( { monkey | items = rest }
            , throwTo
            , level2
            )


turnStep : ( List Monkey, List Monkey ) -> ( List Monkey, List Monkey )
turnStep ( toMove, moved ) =
    let
        pushItem : Int -> Int -> Monkey -> Monkey
        pushItem number item monkey =
            if monkey.number == number then
                { monkey | items = monkey.items ++ [ item ] }

            else
                monkey
    in
    case toMove of
        [] ->
            ( toMove, moved )

        monkey :: rest ->
            if monkey.items == [] then
                ( rest, monkey :: moved )

            else
                let
                    ( newMonkey, number, item ) =
                        doTurn monkey

                    ( newToMove, newMoved ) =
                        ( List.map (pushItem number item) rest
                        , List.map (pushItem number item) moved
                        )
                in
                if newMonkey.items == [] then
                    ( newToMove
                    , newMonkey :: newMoved
                    )

                else
                    ( newMonkey :: newToMove
                    , newMoved
                    )


{-| monkey.number -> count
-}
type alias FlingDict =
    Dict Int Int


type alias State =
    { flingDict : FlingDict
    , lastToMoveCount : Int
    , toMove : List Monkey
    , moved : List Monkey
    }


stateStep : State -> State
stateStep state =
    let
        moveCount =
            List.length state.toMove

        flingDict =
            if state.lastToMoveCount == moveCount then
                state.flingDict

            else
                case state.toMove of
                    monkey :: _ ->
                        let
                            i =
                                log "Monkey" monkey.number

                            itemCount =
                                List.length monkey.items

                            oldCnt =
                                case Dict.get monkey.number state.flingDict of
                                    Nothing ->
                                        0

                                    Just cnt ->
                                        cnt
                        in
                        Dict.insert monkey.number
                            (oldCnt + itemCount)
                            state.flingDict

                    _ ->
                        state.flingDict

        ( toMove, moved ) =
            turnStep ( state.toMove, state.moved )
    in
    { state
        | flingDict = flingDict
        , lastToMoveCount = moveCount
        , toMove = toMove
        , moved = moved
    }


stateLoop : Int -> State -> State
stateLoop rounds state =
    case state.toMove of
        [] ->
            if rounds <= 1 then
                state

            else
                stateLoop (rounds - 1)
                    { state
                        | lastToMoveCount = 0
                        , toMove = List.reverse state.moved
                        , moved = []
                    }

        _ ->
            stateLoop rounds <| stateStep state


log : String -> x -> x
log s x =
    --Debug.log s x
    x


part1 : String -> String
part1 input =
    parseMonkeys input
        |> Debug.log "monkeys"
        |> (\monkeys ->
                let
                    state =
                        stateLoop 20
                            { flingDict = Dict.empty
                            , lastToMoveCount = 0
                            , toMove = monkeys
                            , moved = []
                            }
                in
                state.flingDict
                    |> Dict.toList
                    |> Debug.log "flingDict"
                    |> List.map Tuple.second
                    |> List.sortWith (\x y -> compare y x)
                    |> List.take 2
                    |> Debug.log "top two"
                    |> List.foldl (*) 1
                    |> String.fromInt
           )


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
