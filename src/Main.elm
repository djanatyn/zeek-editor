module Main exposing (main)

import Browser
import Css exposing (..)
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Styled as Styled
import Html.Styled.Attributes exposing (css, href, src)
import List exposing (concat, repeat)


type alias Model =
    { zeekExe : Maybe File
    , levels : Levels
    , log : List String
    }


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias TileUpdate =
    { levelIndex : Int
    , tileIndex : ( Int, Int )
    , tile : Tile
    }


type Msg
    = LoadZeek
    | Log String
    | ZeekLoaded File
    | ChangeLevel Int
    | ModifyTile TileUpdate


loadZeekExe : Cmd Msg
loadZeekExe =
    Select.file [ "application/x-dosexec" ] ZeekLoaded


init : () -> ( Model, Cmd Msg )
init _ =
    ( { zeekExe = Nothing
      , levels = []
      , log = [ "> welcome to zeek editor" ]
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadZeek ->
            ( model, loadZeekExe )

        ChangeLevel level ->
            -- TODO: check bounds
            ( model, Cmd.none )

        ModifyTile { levelIndex, tileIndex, tile } ->
            ( model, Cmd.none )

        ZeekLoaded file ->
            ( { model | zeekExe = Just file, log = "> loaded ZEEK1.EXE" :: model.log }, Cmd.none )

        Log line ->
            ( { model | log = line :: model.log }, Cmd.none )


type alias Levels =
    List Map


type Tile
    = BrickBlue
    | BrickBrown
    | BrickBlueBrown
    | BrickRed
    | BrickGrey
    | BrickYellow
    | SmallBrickBlue
    | SmallBrickRed1
    | SmallBrickRed2
    | SmallBrickBrown1
    | SmallBrickBrown2
    | BrickPurple
    | Zeek
    | YellowFlower
    | BlueFlower
    | Mushroom
    | PoisonMushroom
    | BlueEgg
    | TulipClosed
    | WormApple
    | Disc
    | Apple
    | Treasure
    | Key
    | LockedDoor
    | YellowBall
    | XX
    | BlueHexagon
    | TulipOpen
    | Dino
    | Explosive
    | Eye
    | Floor


enumTile : List Tile
enumTile =
    [ BrickBlue
    , BrickBrown
    , BrickBlueBrown
    , BrickRed
    , BrickGrey
    , BrickYellow
    , SmallBrickBlue
    , SmallBrickRed1
    , SmallBrickRed2
    , SmallBrickBrown1
    , SmallBrickBrown2
    , BrickPurple
    , Zeek
    , YellowFlower
    , BlueFlower
    , Mushroom
    , PoisonMushroom
    , BlueEgg
    , TulipClosed
    , WormApple
    , Disc
    , Apple
    , Treasure
    , Key
    , LockedDoor
    , YellowBall
    , XX
    , BlueHexagon
    , TulipOpen
    , Dino
    , Explosive
    , Eye
    , Floor
    ]


type alias Map =
    List MapRow


type alias MapRow =
    List Tile


tileString : Tile -> String
tileString tile =
    case tile of
        BrickBlue ->
            "BrickBlue"

        BrickBrown ->
            "BrickBrown"

        BrickBlueBrown ->
            "BrickBlueBrown"

        BrickRed ->
            "BrickRed"

        BrickGrey ->
            "BrickGrey"

        BrickYellow ->
            "BrickYellow"

        SmallBrickBlue ->
            "SmallBrickBlue"

        SmallBrickRed1 ->
            "SmallBrickRed1"

        SmallBrickRed2 ->
            "SmallBrickRed2"

        SmallBrickBrown1 ->
            "SmallBrickBrown1"

        SmallBrickBrown2 ->
            "SmallBrickBrown2"

        BrickPurple ->
            "BrickPurple"

        Zeek ->
            "Zeek"

        YellowFlower ->
            "YellowFlower"

        BlueFlower ->
            "BlueFlower"

        Mushroom ->
            "Mushroom"

        PoisonMushroom ->
            "PoisonMushroom"

        BlueEgg ->
            "BlueEgg"

        TulipClosed ->
            "TulipClosed"

        WormApple ->
            "WormApple"

        Disc ->
            "Disc"

        Apple ->
            "Apple"

        Treasure ->
            "Treasure"

        Key ->
            "Key"

        LockedDoor ->
            "LockedDoor"

        YellowBall ->
            "YellowBall"

        XX ->
            "XX"

        BlueHexagon ->
            "BlueHexagon"

        TulipOpen ->
            "TulipOpen"

        Dino ->
            "Dino"

        Explosive ->
            "Explosive"

        Eye ->
            "Eye"

        Floor ->
            "Floor"


tilePosition : Tile -> Int
tilePosition tile =
    case tile of
        BrickBlue ->
            0

        BrickBrown ->
            1

        BrickBlueBrown ->
            2

        BrickRed ->
            3

        BrickGrey ->
            4

        BrickYellow ->
            5

        SmallBrickBlue ->
            6

        SmallBrickRed1 ->
            7

        SmallBrickRed2 ->
            8

        SmallBrickBrown1 ->
            9

        SmallBrickBrown2 ->
            10

        BrickPurple ->
            11

        Zeek ->
            12

        YellowFlower ->
            13

        BlueFlower ->
            14

        Mushroom ->
            15

        PoisonMushroom ->
            16

        BlueEgg ->
            17

        TulipClosed ->
            18

        WormApple ->
            19

        Disc ->
            20

        Apple ->
            21

        Treasure ->
            22

        Key ->
            23

        LockedDoor ->
            24

        YellowBall ->
            25

        XX ->
            26

        BlueHexagon ->
            27

        TulipOpen ->
            28

        Dino ->
            29

        Explosive ->
            30

        Eye ->
            31

        Floor ->
            32


-- Each tile is 36x36 pixels on a vertical spritesheet.
tileStyle : Tile -> Attribute msg
tileStyle tile =
    let
        position =
            "0px " ++ String.fromInt (-36 * tilePosition tile) ++ "px"
    in
    style "background-position" position


emptyMap : Map
emptyMap =
    let
        row =
            concat [ [ BrickBlue ], List.repeat 15 Floor, [ BrickBlue ] ]
    in
    concat
        [ [ List.repeat 17 BrickBlue ]
        , [ concat [ [ BrickBlue, Zeek ], List.repeat 14 Floor, [ BrickBlue ] ] ]
        , List.repeat 8 row
        , [ List.repeat 17 BrickBlue ]
        ]


rowToDiv : MapRow -> Html Msg
rowToDiv row =
    div [ class "map_row" ] (List.map block row)


mapToHtml : Map -> Html Msg
mapToHtml rows =
    div [ class "map" ] (List.map rowToDiv rows)


block : Tile -> Html Msg
block tile =
    div [ tileStyle tile, class "sprite", onClick (Log ("> clicked " ++ tileString tile)) ] []


toolbox : Html Msg
toolbox =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "gap" "20px"
        ]
        [ div [ id "console_buttons" ]
            [ button
                [ onClick LoadZeek
                , style "margin" "5px"
                , style "padding" "5px"
                ]
                [ text "load ZEEK1.EXE" ]
            , button
                [ style "margin" "5px"
                , style "padding" "5px"
                ]
                [ text "save changes" ]
            ]
        , div
            [ id "toolbox"
            , style "display" "grid"
            , style "gap" "5px"
            , style "grid-template-columns" "repeat(5, 1fr)"
            , style "grid-auto-rows" "minmax(36px, auto)"
            ]
            (List.map block enumTile)
        ]


logLine : String -> Html Msg
logLine line =
    Html.pre [] [ text line ]


console : List String -> Html Msg
console log =
    div
        [ id "console"
        , style "overflow" "scroll"
        , style "min-width" "250px"
        ]
        [ div
            [ id "log"
            , style "background-color" "#3f3f3f"
            , style "color" "#ffffff"
            , style "padding" "5px"
            , style "font-family" "monospace"
            , style "min-height" "396px"
            , style "max-height" "396px"
            , style "overflow" "scroll"
            ]
            (List.map logLine log)
        ]


view : Model -> Html Msg
view { log } =
    div
        [ class "container"
        , style "display" "flex"
        , style "flex-direction" "row"
        , style "justify-content" "center"
        , style "gap" "20px"
        , style "align-items" "center"
        , style "min-height" "100vh"
        ]
        [ toolbox
        , mapToHtml emptyMap
        , console log
        ]
