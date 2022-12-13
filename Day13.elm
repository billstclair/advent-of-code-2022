----------------------------------------------------------------
--
-- Day13.elm
-- 2022 Advent of Code
-- https://adventofcode.com/2022/day/13
-- Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Day13 exposing (..)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, code, div, h2, p, pre, span, text, textarea)
import Html.Attributes exposing (cols, href, rows, style, target, value)
import Html.Events exposing (onInput)
import List.Extra as LE
import Parser exposing (Parser, Trailing(..))
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
    { day = "Day 13" ++ partSuffix
    , aocUrl = "https://adventofcode.com/2022/day/13"
    , githubUrl = "https://github.com/billstclair/advent-of-code-2022/blob/main/Day13.elm"
    }


type Packet
    = IntPacket Int
    | ListPacket (List Packet)
    | BadPacket String


type Pair
    = Pair Packet Packet
    | BadPair (List String)


comparePackets : Packet -> Packet -> Order
comparePackets pack1 pack2 =
    case pack1 of
        IntPacket i1 ->
            case pack2 of
                IntPacket i2 ->
                    if i1 < i2 then
                        LT

                    else if i1 == i2 then
                        EQ

                    else
                        GT

                _ ->
                    comparePackets (ListPacket [ pack1 ]) pack2

        ListPacket list1 ->
            case pack2 of
                IntPacket _ ->
                    comparePackets pack1 <| ListPacket [ pack2 ]

                ListPacket list2 ->
                    let
                        loop : List Packet -> List Packet -> Order
                        loop l1t l2t =
                            case l1t of
                                [] ->
                                    if l2t == [] then
                                        EQ

                                    else
                                        LT

                                p1 :: l1rest ->
                                    case l2t of
                                        [] ->
                                            GT

                                        p2 :: l2rest ->
                                            case comparePackets p1 p2 of
                                                LT ->
                                                    LT

                                                GT ->
                                                    GT

                                                EQ ->
                                                    loop l1rest l2rest
                    in
                    loop list1 list2

                _ ->
                    GT

        _ ->
            GT


isCorrectOrder : Pair -> Bool
isCorrectOrder pair =
    case pair of
        Pair p1 p2 ->
            comparePackets p1 p2 /= GT

        _ ->
            False


parsePair : String -> Pair
parsePair string =
    case String.split "\n" string of
        [ left, right ] ->
            let
                ( lp, rp ) =
                    ( parsePacket left, parsePacket right )
            in
            case lp of
                BadPacket s ->
                    BadPair [ s, Debug.toString rp ]

                _ ->
                    case rp of
                        BadPacket s ->
                            BadPair [ Debug.toString lp, s ]

                        _ ->
                            Pair lp rp

        strings ->
            BadPair strings


packetParser : Parser Packet
packetParser =
    Parser.oneOf
        [ Parser.int
            |> Parser.andThen (Parser.succeed << IntPacket)
        , Parser.sequence
            { start = "["
            , separator = ","
            , end = "]"
            , spaces = Parser.spaces
            , item = Parser.lazy (\_ -> packetParser)
            , trailing = Forbidden
            }
            |> Parser.andThen (Parser.succeed << ListPacket)
        ]


parsePacket : String -> Packet
parsePacket string =
    case Parser.run packetParser string of
        Ok packet ->
            packet

        _ ->
            BadPacket string


badPairp : Pair -> Bool
badPairp pair =
    case pair of
        BadPair _ ->
            True

        _ ->
            False


part1 : String -> String
part1 input =
    let
        pairs =
            log "pairs"
                (String.split "\n\n" input
                    |> List.map parsePair
                )

        badPairs =
            List.filter badPairp pairs
    in
    if List.length badPairs > 0 then
        Debug.toString badPairs

    else
        List.indexedMap (\idx pair -> ( idx + 1, isCorrectOrder pair )) pairs
            |> log "isCorrectorders"
            |> List.filter (\( _, correct ) -> correct)
            |> log "only correct"
            |> List.foldl (\( idx, _ ) sum -> idx + sum) 0
            |> String.fromInt


divider1 : Packet
divider1 =
    parsePacket "[[2]]"


divider2 : Packet
divider2 =
    parsePacket "[[6]]"


pairPackets : Pair -> List Packet
pairPackets pair =
    case pair of
        Pair p1 p2 ->
            [ p1, p2 ]

        _ ->
            []


part2 : String -> String
part2 input =
    let
        pairs =
            String.split "\n\n" input
                |> List.map parsePair
                |> log "pairs"

        badPairs =
            List.filter badPairp pairs
    in
    if List.length badPairs > 0 then
        Debug.toString badPairs

    else
        let
            packets =
                divider1
                    :: divider2
                    :: List.foldr
                        (\pair list ->
                            List.append (pairPackets pair) list
                        )
                        []
                        pairs

            sortedPackets =
                List.sortWith comparePackets packets
                    |> log "sortedPackets"

            index1 =
                1 + (Maybe.withDefault 0 <| LE.elemIndex divider1 sortedPackets)

            index2 =
                1 + (Maybe.withDefault 0 <| LE.elemIndex divider2 sortedPackets)
        in
        (index1 * index2)
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
