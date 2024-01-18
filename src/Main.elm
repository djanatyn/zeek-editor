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
    = YellowBrick
    | BlockYellow1
    | BlockYellow2
    | BlockGrey1
    | BlockGrey2


type alias Map =
    List MapRow


type alias MapRow =
    List Tile


blockClass : Tile -> String
blockClass tile =
    case tile of
        YellowBrick ->
            "sprite block_yellow_brick"

        BlockYellow1 ->
            "sprite block_yellow_1"

        BlockYellow2 ->
            "sprite block_yellow_2"

        BlockGrey1 ->
            "sprite block_grey_1"

        BlockGrey2 ->
            "sprite block_grey_2"


emptyMap : Map
emptyMap =
    let
        emptyRow =
            concat [ [ YellowBrick ], List.repeat 8 BlockYellow1, [ YellowBrick ] ]
    in
    concat
        [ [ List.repeat 10 YellowBrick ]
        , List.repeat 8 emptyRow
        , [ List.repeat 10 YellowBrick ]
        ]


rowToDiv : MapRow -> Html Msg
rowToDiv row =
    div [ class "map_row" ] (List.map block row)


mapToHtml : Map -> Html Msg
mapToHtml rows =
    div [ class "map" ] (List.map rowToDiv rows)


block : Tile -> Html Msg
block tile =
    div [ class (blockClass tile) ] []


view : Model -> Html Msg
view _ =
    div [ class "container" ]
        [ mapToHtml emptyMap
        , button [ onClick LoadZeek ] [ text "load ZEEK1.EXE" ]
        ]
