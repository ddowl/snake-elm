module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Debug exposing (log)
import Html exposing (..)
import Keyboard exposing (RawKey)
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
    | KeyDown RawKey


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model | snake = moveSnake model.snake model.direction }, Cmd.none )

        KeyDown rawKey ->
            case Keyboard.anyKeyOriginal rawKey of
                Just Keyboard.ArrowUp ->
                    ( { model | direction = Up }, Cmd.none )

                Just Keyboard.ArrowDown ->
                    ( { model | direction = Down }, Cmd.none )

                Just Keyboard.ArrowLeft ->
                    ( { model | direction = Left }, Cmd.none )

                Just Keyboard.ArrowRight ->
                    ( { model | direction = Right }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


moveSnake snake direction =
    List.map (nextSnakePart direction) snake


growSnake snake direction =
    let
        head =
            case List.head snake of
                Just h ->
                    h

                -- This case should never happen. How can I structure this better?
                Nothing ->
                    { x = 0, y = 0 }

        nextHead =
            nextSnakePart direction head
    in
    nextHead :: snake


nextSnakePart direction =
    case direction of
        Up ->
            \pos -> { x = pos.x, y = pos.y - 1 }

        Down ->
            \pos -> { x = pos.x, y = pos.y + 1 }

        Left ->
            \pos -> { x = pos.x - 1, y = pos.y }

        Right ->
            \pos -> { x = pos.x + 1, y = pos.y }

        None ->
            \pos -> pos


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 100 Tick
        , Keyboard.downs KeyDown
        ]



-- VIEW


cellSize =
    30


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
    div [ width "100%", height "100%" ]
        [ h1 [] [ Html.text "Snake!" ]
        , p [] [ Html.text "Grab the food, don't hit the walls!" ]
        , svg [ viewBox ("0 0 " ++ gridWidthStr ++ " " ++ gridHeightStr), width gridWidthStr, height gridHeightStr ]
            ([ background, cell "red" model.food ] ++ List.map (cell "green") model.snake)
        ]


background =
    rect
        [ width gridWidthStr
        , height gridHeightStr
        , fill "#997B1F"
        ]
        []


cell color position =
    rect
        [ x (String.fromInt (position.x * cellSize))
        , y (String.fromInt (position.y * cellSize))
        , width cellSizeStr
        , height cellSizeStr
        , fill color
        , stroke "gray"
        ]
        []
