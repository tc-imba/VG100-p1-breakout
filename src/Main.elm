module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (Html, text, div, h1, img, button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (src)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Keyboard exposing (RawKey, Key(..))
import Keyboard.Arrows
import Time
import Objects.Ball exposing (ball)


---- MODEL ----


type alias Model =
    GameState


type GameState
    = NotPlaying GameModel
    | Playing GameModel
    | Won GameModel
    | Lost GameModel


type alias GameModel =
    { ballPosition: (Float, Float)
    , ballVelocity: (Float, Float)
    --, pressedKeys: List Key
    }


initGameModel : GameModel
initGameModel =
    { ballPosition = (50, 75)
    , ballVelocity = (0,0)
    --, pressedKeys = []
    }


init : ( Model, Cmd Msg )
init =
    ( NotPlaying initGameModel, Cmd.none )



---- UPDATE ----


type Msg
    = TimeUpdate Float
    | GameInput GameControl Bool
    | KeyboardMsg Keyboard.Msg
    | StartMoving
    | StopMoving
    | NoOp


type GameControl
    = Start
    | Pause
    | Reset
    | PaddleLeft
    | PaddleRight


keyToGameControl : Bool -> RawKey -> Msg
keyToGameControl isDown key =
    case (Keyboard.anyKeyOriginal key) of
        Just Keyboard.ArrowLeft ->
            GameInput PaddleLeft isDown
        Just Keyboard.ArrowRight ->
            GameInput PaddleRight isDown
        Just Keyboard.Spacebar ->
            GameInput Start isDown
        Just Keyboard.Shift ->
            GameInput Reset isDown
        _ ->
            NoOp



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        --KeyboardMsg keyMsg ->
        --    case model of
        --        NotPlaying gameModel ->
        --            ( NotPlaying { gameModel | pressedKeys = Keyboard.update keyMsg gameModel.pressedKeys}
        --            , Cmd.none
        --            )
        --        _ ->
        --            (model, Cmd.none)
        GameInput Start isDown ->
            case isDown of
                True ->
                    case model of
                        NotPlaying gameModel ->
                            let
                                newModel = { gameModel | ballVelocity = (10,10) }
                            in
                            ( Playing newModel, Cmd.none )
                        _ ->
                            (model, Cmd.none)
                _ ->
                    (model, Cmd.none)

        GameInput Pause isDown ->
            case isDown of
                True ->
                    case model of
                        Playing gameModel ->
                            let
                                newModel = { gameModel | ballVelocity = (0,0) }
                            in
                            ( NotPlaying newModel, Cmd.none )
                        _ ->
                            (model, Cmd.none)
                _ ->
                    (model, Cmd.none)

        --GameInput Reset isDown ->
        --    case isDown of
        --        True ->
        --            case

        TimeUpdate dt ->
            case model of
                Playing gameModel ->
                    updateGameDisplay dt gameModel
                _ ->
                    (model, Cmd.none)
        _ ->
            (model, Cmd.none)


determineVelocity : GameModel -> (Float, Float)
determineVelocity gameModel =
    let
        ( ballPositionX, ballPositionY ) =
            gameModel.ballPosition
        ( ballVelocityX, ballVelocityY ) =
            gameModel.ballVelocity
    in
    if ballPositionX <= 2 || ballPositionX >=98 then
        ( -ballVelocityX, ballVelocityY )
    else if ballPositionY <= 2 || ballPositionY >= 75 then
        ( ballVelocityX, -ballVelocityY )
    else
        ( ballVelocityX, ballVelocityY )





---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container"]
        [ div []
            [ button [ onClick (GameInput Start True)] [ Html.text "Start moving!"]
            , button [ onClick (GameInput Pause True)] [ Html.text "Stop moving!"]
            ]
        , div []
            [ gameBoard model ]
        ]


gameBoard : Model -> Html Msg
gameBoard model =
    case model of
        NotPlaying gameModel ->
            displayGameBoard gameModel
        Playing gameModel ->
            displayGameBoard gameModel
        Won gameModel ->
            displayGameBoard gameModel
        Lost gameModel ->
            displayGameBoard gameModel


displayGameBoard : GameModel -> Html Msg
displayGameBoard gameModel =
    Svg.svg
        [ width "100%"
        , height "100%"
        , viewBox "0 0 100 77"
        ]
        [ rect
            [ x "0"
            , y "0"
            , width "100"
            , height "77"
            , fill "#ffcb0b"
            ]
            []
        , ball gameModel.ballPosition
        ]


updateGameDisplay : Float -> GameModel -> ( Model, Cmd Msg )
updateGameDisplay dt gameModel =
    let
        ( ballPositionX, ballPositionY ) =
            gameModel.ballPosition
        ( ballVelocityX, ballVelocityY ) =
            determineVelocity gameModel
    in
    ( Playing
        { gameModel
            | ballPosition =
                ( ballPositionX + ballVelocityX / dt
                , ballPositionY + ballVelocityY / dt
                )
            , ballVelocity =
                ( ballVelocityX
                , ballVelocityY
                )
        }
    , Cmd.none
    )



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta TimeUpdate
        , Keyboard.downs <| keyToGameControl True
        , Keyboard.ups <| keyToGameControl False
        ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
