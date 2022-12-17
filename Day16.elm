----------------------------------------------------------------
--
-- Day16.elm
-- 2022 Advent of Code
-- https://adventofcode.com/2022/day/16
-- Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Day16 exposing (..)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, code, div, h2, input, p, pre, span, text, textarea)
import Html.Attributes exposing (checked, cols, href, rows, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import List.Extra as LE



{- Customize below here for each puzzle. -}


realIsUsed : Bool
realIsUsed =
    False


defaultIsReal : Bool
defaultIsReal =
    False


defaultPart : Int
defaultPart =
    1


partSuffix : Model -> String
partSuffix model =
    if model.part == 0 then
        ""

    else
        ", part " ++ String.fromInt model.part


dayStrings =
    { day = "Day 16"
    , aocUrl = "https://adventofcode.com/2022/day/16"
    , githubUrl = "https://github.com/billstclair/advent-of-code-2022/blob/main/Day16.elm"
    }


type alias Valve =
    { name : String
    , flowRate : Int
    , tunnels : List String
    }


{-| valve.name -> valve
-}
type alias Network =
    Dict String Valve


parseValve : String -> Maybe Valve
parseValve line =
    let
        name =
            String.split " " line
                |> LE.getAt 1
                |> Maybe.withDefault ""

        flowRate =
            String.split "=" line
                |> LE.getAt 1
                |> Maybe.withDefault "0"
                |> String.split ";"
                |> List.head
                |> Maybe.withDefault "0"
                |> String.toInt
                |> Maybe.withDefault 0

        valves =
            String.split "valves " line
                |> LE.getAt 1
                |> Maybe.withDefault ""
                |> String.split ", "
                |> List.filter ((/=) "")

        valve =
            String.split "valve " line
                |> LE.getAt 1
                |> Maybe.withDefault ""
                |> (\v -> [ v ])
                |> List.filter ((/=) "")

        allValves =
            List.append valves valve
    in
    if name == "" || allValves == [] then
        Nothing

    else
        Just
            { name = name
            , flowRate = flowRate
            , tunnels = allValves
            }


makeNetwork : List Valve -> ( String, Network )
makeNetwork valves =
    ( case List.head valves of
        Nothing ->
            ""

        Just valve ->
            valve.name
    , List.foldl (\valve network -> Dict.insert valve.name valve network)
        Dict.empty
        valves
    )


totalTime : Int
totalTime =
    30


type alias Path =
    { pressure : Int
    , path : List String
    }


emptyPath : Path
emptyPath =
    { pressure = 0
    , path = []
    }


traverse : ( String, Network ) -> Path
traverse ( name, network ) =
    let
        descend : Int -> String -> Path -> Path
        descend timeLeft nam path =
            if List.member nam path.path then
                path

            else
                let
                    path2 =
                        { path | path = nam :: path.path }

                    trash =
                        log "descend" ( timeLeft, nam, path2 )
                in
                if timeLeft < 2 then
                    log "  path2" path2

                else
                    case Dict.get nam network of
                        Nothing ->
                            -- Shouldn't happen
                            let
                                n =
                                    log "No valve named" nam
                            in
                            path2

                        Just { flowRate, tunnels } ->
                            let
                                ( tl, pressure ) =
                                    if flowRate <= 0 then
                                        ( timeLeft - 1, 0 )

                                    else
                                        ( timeLeft - 2, flowRate * (timeLeft - 1) )

                                loop : List String -> Path -> Path
                                loop names sol =
                                    case log "loop names" names of
                                        [] ->
                                            log "  sol" sol

                                        n :: rest ->
                                            let
                                                sol2 =
                                                    descend tl
                                                        n
                                                        { sol
                                                            | pressure =
                                                                sol.pressure
                                                                    + pressure
                                                        }
                                            in
                                            if sol2.pressure >= sol.pressure then
                                                loop rest sol2

                                            else
                                                loop rest sol
                            in
                            loop tunnels path2
    in
    descend totalTime name emptyPath


part1 : Model -> String -> String
part1 model input =
    String.split "\n" input
        |> List.filterMap parseValve
        |> makeNetwork
        |> log "makeNetwork"
        |> traverse
        |> Debug.toString


part2 : Model -> String -> String
part2 model input =
    input


solve : Model -> String -> String
solve model input =
    if model.part == 1 then
        part1 model input

    else
        part2 model input


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
    , part : Int
    , isReal : Bool
    }


type Msg
    = Input String
    | TogglePart
    | ToggleReal


init : Model
init =
    { input = ""
    , output = ""
    , part = defaultPart
    , isReal = defaultIsReal
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input input ->
            { model
                | input = input
                , output = solve model input
            }

        TogglePart ->
            let
                m =
                    { model
                        | part =
                            if model.part == 1 then
                                2

                            else
                                1
                    }
            in
            { m | output = solve m model.input }

        ToggleReal ->
            let
                m =
                    { model
                        | isReal = not model.isReal
                    }
            in
            { m | output = solve m model.input }


checkBox : Msg -> Bool -> String -> Html Msg
checkBox msg isChecked label =
    span
        [ onClick msg
        , style "cursor" "default"
        ]
        [ input
            [ type_ "checkbox"
            , checked isChecked
            ]
            []
        , b label
        ]


view : Model -> Html Msg
view model =
    div [ style "margin-left" "3em" ]
        [ h2 []
            [ text <|
                "Advent of Code 2022 "
                    ++ special.middleDot
                    ++ " "
                    ++ dayStrings.day
                    ++ partSuffix model
            ]
        , p []
            [ checkBox TogglePart (model.part == 1) "part 1"
            , if realIsUsed then
                span []
                    [ br
                    , checkBox ToggleReal model.isReal "real, not example"
                    ]

              else
                text ""
            ]
        , if model.part == 1 then
            text ""

          else
            text ""
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
