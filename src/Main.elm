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
            ( { model | zeekExe = Just file }, Cmd.none )


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


type alias Map =
    List MapRow


type alias MapRow =
    List Tile


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
        emptyRow =
            concat [ [ BrickBlue ], List.repeat 8 Floor, [ BrickBlue ] ]

        border =
            [ List.repeat 10 BrickBlue ]
    in
    concat [ border, List.repeat 8 emptyRow, border ]


rowToDiv : MapRow -> Html Msg
rowToDiv row =
    div [ class "map_row" ] (List.map block row)


mapToHtml : Map -> Html Msg
mapToHtml rows =
    div [ class "map" ] (List.map rowToDiv rows)


block : Tile -> Html Msg
block tile =
    div [ tileStyle tile, class "sprite" ] []


view : Model -> Html Msg
view _ =
    div [ class "container" ]
        [ mapToHtml emptyMap
        , button [ onClick LoadZeek ] [ text "load ZEEK1.EXE" ]
        ]
