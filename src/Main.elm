module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Debug exposing (log)
import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { food : Position
    , snake : List Position
    , direction : Direction
    }


type Direction
    = Up
    | Down
    | Left
    | Right
    | None


type alias Position =
    { x : Int, y : Int }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { food = { x = 5, y = 3 }
      , snake = [ { x = 0, y = 0 } ]
      , direction = None
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            log "model" model
    in
    case msg of
        Tick _ ->
            -- move the food around the grid
            let
                nextX =
                    modBy gridWidth (model.food.x + 1)

                nextY =
                    modBy gridHeight (model.food.y + 1)
            in
            ( { model | food = { x = nextX, y = nextY } }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 300 Tick



-- VIEW


cellSize =
    10


gridWidth =
    20


gridHeight =
    15


gridWidthPixels =
    gridWidth * cellSize


gridHeightPixels =
    gridHeight * cellSize


cellSizeStr =
    String.fromInt cellSize


gridWidthStr =
    String.fromInt gridWidthPixels


gridHeightStr =
    String.fromInt gridHeightPixels


view : Model -> Html Msg
view model =
    svg
        [ class "grid"
        , viewBox ("0 0 " ++ gridWidthStr ++ " " ++ gridHeightStr)
        ]
        (renderBackground ++ renderCell model.food)


renderBackground =
    [ rect
        [ width gridWidthStr
        , height gridHeightStr
        , fill "#997B1F"
        ]
        []
    ]


renderCell position =
    [ rect
        [ x (String.fromInt (position.x * cellSize))
        , y (String.fromInt (position.y * cellSize))
        , width cellSizeStr
        , height cellSizeStr
        , fill "green"
        , stroke "gray"
        ]
        []
    ]
