module Main exposing (main)

import Array exposing (Array)
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
import String exposing (fromInt)


type alias Model =
    { zeekExe : Maybe File
    , currentLevel : Map
    , log : List String
    , selectedTile : Tile
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
    }


type Msg
    = LoadZeek
    | Log String
    | ZeekLoaded File
    | SelectTile Tile
    | ChangeLevel Int
    | ModifyTile TileUpdate


type alias Map =
    Array MapRow


type alias MapRow =
    Array Tile


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


loadZeekExe : Cmd Msg
loadZeekExe =
    Select.file [ "application/x-dosexec" ] ZeekLoaded


init : () -> ( Model, Cmd Msg )
init _ =
    ( { zeekExe = Nothing
      , currentLevel = emptyMap
      , log = [ "> welcome to zeek editor" ]
      , selectedTile = Zeek
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

        SelectTile tile ->
            ( { model | selectedTile = tile, log = ("> selected " ++ tileString tile) :: model.log }, Cmd.none )

        ModifyTile tileUpdate ->
            let
                updatedMap : Maybe Map
                updatedMap =
                    updateTile model.selectedTile tileUpdate model.currentLevel
                line = "> updated " ++ (coord tileUpdate.tileIndex) ++ " to " ++ (tileString model.selectedTile)
            in
            case updatedMap of
                Just updated ->
                    ( { model | currentLevel = updated, log = line :: model.log }, Cmd.none )

                Nothing ->
                    ( { model | log = "> failed" :: model.log }, Cmd.none )

        ZeekLoaded file ->
            ( { model | zeekExe = Just file, log = "> loaded ZEEK1.EXE" :: model.log }, Cmd.none )

        Log line ->
            ( { model | log = line :: model.log }, Cmd.none )


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


updateTile : Tile -> TileUpdate -> Map -> Maybe Map
updateTile newTile { levelIndex, tileIndex } map =
    let
        ( x, y ) =
            tileIndex

        maybeRow : Maybe MapRow
        maybeRow =
            Array.get y map

        updateRow : MapRow -> MapRow
        updateRow =
            Array.set x newTile

        updateMap : MapRow -> Map -> Map
        updateMap updatedRow =
            Array.set y updatedRow
    in
    case maybeRow of
        Just row ->
            Just (updateMap (updateRow row) map)

        Nothing ->
            Nothing




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
        border =
            Array.repeat 17 BrickBlue

        row =
            Array.fromList (concat [ [ BrickBlue ], List.repeat 15 Floor, [ BrickBlue ] ])

        top =
            Array.fromList (concat [ [ BrickBlue, Zeek ], List.repeat 14 Floor, [ BrickBlue ] ])
    in
    Array.fromList
        (concat
            [ [ border, top ]
            , List.repeat 6 row
            , [ border ]
            ]
        )


coord : (Int, Int) -> String
coord (x,y) = "(" ++ fromInt x ++ ", " ++ fromInt y ++ ")"



-- Log ("> clicked " ++ tileString tile ++ " " ++ coord x y))


block : Int -> Int -> Int -> Tile -> Html Msg
block levelIndex y x tile =
    let
        tileUpdate : TileUpdate
        tileUpdate =
            { levelIndex = levelIndex, tileIndex = ( x, y ) }
    in
    div
        [ tileStyle tile
        , class "sprite"
        , onClick (ModifyTile tileUpdate)
        ]
        []


rowToDiv : Int -> Int -> MapRow -> Html Msg
rowToDiv levelIndex y row =
    div [ class "map_row" ] (Array.toList (Array.indexedMap (block levelIndex y) row))


mapToHtml : Int -> Map -> Html Msg
mapToHtml levelIndex rows =
    div [ class "map" ] (Array.toList (Array.indexedMap (rowToDiv levelIndex) rows))


selectedToolboxBlock : Tile -> Html Msg
selectedToolboxBlock tile =
    div
        [ tileStyle tile
        , class "sprite"
        , style "outline" "solid"
        , style "z-index" "2"
        , onClick (Log "> already selected!")
        ]
        []


toolboxBlock : Tile -> Html Msg
toolboxBlock tile =
    div
        [ tileStyle tile
        , class "sprite"
        , onClick (SelectTile tile)
        ]
        []


toolbox : Tile -> Html Msg
toolbox selected =
    let
        renderTile : Tile -> Html Msg
        renderTile tile =
            if tile == selected then
                selectedToolboxBlock tile

            else
                toolboxBlock tile
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "gap" "20px"
        , style "align-items" "center"
        ]
        [ div
            [ id "console_buttons"
            , style "display" "flex"
            , style "flex-direction" "row"
            , style "gap" "5px"
            ]
            [ button
                [ onClick LoadZeek
                , style "padding" "5px"
                ]
                [ text "load ZEEK1.EXE" ]
            , button
                [ style "padding" "5px"
                ]
                [ text "save changes" ]
            ]
        , div
            [ id "toolbox"
            , style "display" "grid"
            , style "grid-template-columns" "38px 38px 38px 38px 38px"
            , style "grid-auto-rows" "38px"
            ]
            (List.map renderTile enumTile)
        ]


logLine : String -> Html Msg
logLine line =
    Html.pre [ style "margin" "0px" ] [ text line ]


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
            , style "min-height" "324px"
            , style "max-height" "324px"
            , style "overflow" "scroll"
            ]
            (List.map logLine log)
        ]


view : Model -> Html Msg
view model =
    div
        [ class "container"
        , style "display" "flex"
        , style "flex-direction" "row"
        , style "justify-content" "center"
        , style "align-items" "center"
        , style "gap" "20px"
        , style "min-height" "100vh"
        ]
        [ toolbox model.selectedTile
        , mapToHtml 1 model.currentLevel
        , console model.log
        ]
