----------------------------------------------------------------
--
-- Day01.elm
-- Day 1 of the 2022 Advent of Code
-- https://adventofcode.com/2022/day/1
-- Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Day01 exposing (main)

import Browser
import Html exposing (Attribute, Html, div, p, text, textarea)
import Html.Attributes exposing (cols, rows, style, value)
import Html.Events exposing (onInput)


{-| Customize this for each puzzle.
-}
solve : String -> String
solve input =
    input


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
        [ p [] [ text "Paste the input below. The solution will be computed." ]
        , textarea
            [ rows 40
            , cols 80
            , value model.input
            , onInput Input
            ]
            []
        , p [] [ b "Solution: " ]
        , p [] [ text model.output ]
        ]


b : String -> Html msg
b string =
    Html.b [] [ text string ]
