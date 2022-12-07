----------------------------------------------------------------
--
-- Day07.elm
-- 2022 Advent of Code
-- https://adventofcode.com/2022/day/7
-- Copyright (c) 2022 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Day07 exposing (main)

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
    { day = "Day 7" ++ partSuffix
    , aocUrl = "https://adventofcode.com/2022/day/7"
    , githubUrl = "https://github.com/billstclair/advent-of-code-2022/blob/main/Day07.elm"
    }


type alias Directory =
    { subdirs : Set String
    , files : Dict String Int
    }


type alias Filesystem =
    { cd : String
    , directories : Dict String Directory
    }


emptyFilesystem : Filesystem
emptyFilesystem =
    { cd = "/"
    , directories = Dict.empty
    }


trimDir : String -> String
trimDir s =
    if String.endsWith "/" s then
        String.dropRight 1 s

    else
        s


subdirString : String -> String -> String
subdirString subdir dir =
    let
        dir2 =
            trimDir dir

        subdir2 =
            if subdir == "/" then
                ""

            else
                subdir
    in
    subdir2 ++ "/" ++ dir2


parseLine : String -> Filesystem -> Filesystem
parseLine line fs =
    if String.left 2 line == "$ " then
        let
            cmd =
                String.dropLeft 2 line

            cdorls =
                String.left 2 cmd
        in
        if cdorls == "cd" then
            let
                dir =
                    String.dropLeft 3 cmd

                ( dir2, newdir ) =
                    if String.left 1 dir == "/" then
                        ( dir, "" )

                    else if dir == ".." then
                        if fs.cd == "/" then
                            ( fs.cd, "" )

                        else
                            let
                                cd =
                                    SE.leftOfBack "/" fs.cd
                            in
                            if cd == "" then
                                ( "/", "" )

                            else
                                ( cd, "" )

                    else
                        ( subdirString fs.cd dir, dir )

                directories =
                    if newdir == "" then
                        fs.directories

                    else
                        case Dict.get fs.cd fs.directories of
                            Just directory ->
                                -- They could get tricky and CD to a directory we haven't seen
                                Dict.insert fs.cd
                                    { directory
                                        | subdirs =
                                            Set.insert dir directory.subdirs
                                    }
                                    fs.directories

                            Nothing ->
                                fs.directories
            in
            { fs
                | cd = dir2
                , directories = directories
            }

        else
            fs

    else
        case String.split " " line of
            [ "dir", dir ] ->
                let
                    directory =
                        case Dict.get fs.cd fs.directories of
                            Nothing ->
                                { subdirs = Set.fromList [ dir ]
                                , files = Dict.empty
                                }

                            Just d ->
                                { d | subdirs = Set.insert dir d.subdirs }
                in
                { fs
                    | directories =
                        Dict.insert fs.cd directory fs.directories
                }

            [ sizeString, filename ] ->
                case String.toInt sizeString of
                    Nothing ->
                        fs

                    Just size ->
                        let
                            directory =
                                case Dict.get fs.cd fs.directories of
                                    Nothing ->
                                        { subdirs = Set.fromList []
                                        , files =
                                            Dict.fromList [ ( filename, size ) ]
                                        }

                                    Just d ->
                                        { d
                                            | files =
                                                Dict.insert filename
                                                    size
                                                    d.files
                                        }
                        in
                        { fs
                            | directories =
                                Dict.insert fs.cd directory fs.directories
                        }

            _ ->
                fs


directoryFileSize : Directory -> Int
directoryFileSize directory =
    let
        folder name size total =
            size + total
    in
    Dict.foldl folder 0 directory.files


directoryTotalSize : String -> Filesystem -> Int
directoryTotalSize dir fs =
    case Dict.get dir fs.directories of
        Nothing ->
            0

        Just directory ->
            let
                folder subdir total =
                    total + directoryTotalSize (subdirString dir subdir) fs
            in
            directoryFileSize directory
                + Set.foldl folder 0 directory.subdirs


solve : String -> String
solve input =
    let
        lines =
            String.split "\n" input

        fs =
            List.foldl parseLine emptyFilesystem lines

        folder dir _ res =
            ( dir, directoryTotalSize dir fs ) :: res

        sizes =
            Dict.foldl folder [] fs.directories
    in
    if part == 1 then
        part1 sizes

    else
        part2 sizes


part1 : List ( String, Int ) -> String
part1 sizes =
    let
        sizeFolder ( _, size ) total =
            if size <= 100000 then
                total + size

            else
                total
    in
    List.foldr sizeFolder 0 sizes
        |> String.fromInt


diskSize : Int
diskSize =
    70000000


spaceNeeded : Int
spaceNeeded =
    30000000


part2 : List ( String, Int ) -> String
part2 sizes =
    let
        sizeDict =
            Dict.fromList sizes
    in
    case Dict.get "/" sizeDict of
        Nothing ->
            "Can't find '/' directory!"

        Just totalUsed ->
            let
                freeSpace =
                    diskSize - totalUsed

                toDelete =
                    spaceNeeded - freeSpace
            in
            if toDelete <= 0 then
                "There's enough free space: "
                    ++ String.fromInt freeSpace
                    ++ " > "
                    ++ String.fromInt spaceNeeded

            else
                List.filter (\( dir, size ) -> size >= toDelete) sizes
                    |> List.sortBy Tuple.second
                    |> List.head
                    |> Maybe.withDefault ( "No directory big enough", 0 )
                    |> Debug.log "Best (dir, size)"
                    |> Tuple.second
                    |> String.fromInt


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
