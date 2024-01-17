module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import List exposing (concat, repeat)


type alias Model =
    Int


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = Null


initialValue : Model
initialValue =
    0


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialValue, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


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
            concat [ [ YellowBrick ], repeat 3 BlockYellow1, [ YellowBrick ] ]
    in
    [ repeat 5 YellowBrick
    , emptyRow
    , emptyRow
    , emptyRow
    , repeat 5 YellowBrick
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
    mapToHtml emptyMap
