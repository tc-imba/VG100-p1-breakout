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
import Array
import List
import Objects.Ball exposing (ball)
import Objects.Paddle exposing (paddle)
import Objects.Background exposing (background, backgroundtext)
import Objects.Blocks exposing (allBlocks)


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
    , ballRadius: Float
    , ballMovingSpeed: (Float, Float)
    , paddlePosition: (Float, Float)
    , paddleVelocityX: Float
    , paddleMovingSpeed: Float
    , paddleSize: (Float, Float)
    , windowSize: (Float, Float)
    , blockRange: (Float, Float)
    , blockNumber: (Int, Int)
    , blockSize: (Float, Float)
    , blocks: Array.Array (Array.Array Bool)
    --, pressedKeys: List Key
    }


initGameModel : GameModel
initGameModel =
    let
        blockRangeX = 100
        blockRangeY = 20
        blockNumberX = 10
        blockNumberY = 5
    in
    { ballPosition = (25, 70)
    , ballVelocity = (0,0)
    , ballRadius = 2
    , ballMovingSpeed = (10,10)
    , paddlePosition = (45, 70)
    , paddleVelocityX = 0
    , paddleMovingSpeed = 2
    , paddleSize = (20, 2)
    , windowSize = (100, 77)
    , blockRange = (blockRangeX, blockRangeY)
    , blockNumber = (blockNumberX, blockNumberY)
    , blockSize = ( blockRangeX / blockNumberX, blockRangeY / blockNumberY )
    , blocks = Array.repeat blockNumberX <| Array.repeat blockNumberY True
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
        Just Keyboard.Enter ->
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
                                newModel =
                                    { gameModel
                                    | ballVelocity =
                                        ( Tuple.first gameModel.ballMovingSpeed
                                        , -1 * ( Tuple.second gameModel.ballMovingSpeed)
                                        )
                                    }
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

        GameInput Reset isDown ->
            case isDown of
                True ->
                    ( NotPlaying initGameModel, Cmd.none )
                _ ->
                    (model, Cmd.none)

        GameInput PaddleLeft isDown ->
            case model of
                Playing gameModel ->
                    let
                        newModel = { gameModel | paddleVelocityX = if isDown then -gameModel.paddleMovingSpeed else 0 }
                    in
                        ( Playing newModel, Cmd.none)
                _ ->
                    ( model, Cmd.none )

        GameInput PaddleRight isDown ->
            case model of
                Playing gameModel ->
                    let
                        newModel = { gameModel | paddleVelocityX = if isDown then gameModel.paddleMovingSpeed else 0 }
                    in
                        ( Playing newModel, Cmd.none)
                _ ->
                    ( model, Cmd.none )

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
        ( ballPositionX, ballPositionY ) = gameModel.ballPosition
        ( ballSpeedX, ballSpeedY ) = gameModel.ballMovingSpeed
        ( ballVelocityX, ballVelocityY ) = gameModel.ballVelocity
        ( paddlePositionX, paddlePositionY ) = gameModel.paddlePosition
        ( paddleWidth, paddleHeight ) = gameModel.paddleSize
        r = gameModel.ballRadius
        ( windowWidth, windowHeight ) = gameModel.windowSize
        newBallVelocity =
            ( if ballPositionX <= r then ballSpeedX
              else if ballPositionX >= windowWidth - r then
                -ballSpeedX
              else if ballPositionX >= paddlePositionX && ballPositionX <= paddlePositionX + 0.5 && ballPositionY >= paddlePositionY && ballPositionY <= paddlePositionY + paddleHeight then
                -ballSpeedX
              else if ballPositionX <= paddlePositionX + paddleWidth && ballPositionX >= paddlePositionX + paddleWidth - 0.5 && ballPositionY >= paddlePositionY && ballPositionY <= paddlePositionY + paddleHeight then
                ballSpeedX
              else ballVelocityX
            , if ballPositionY <= r then ballSpeedY
              else if ballPositionY >= paddlePositionY - r && ballPositionY <= paddlePositionY - r + 1 && ballPositionX >= paddlePositionX && ballPositionX <= paddlePositionX + paddleWidth then
                -ballSpeedY
              else if ballPositionY >= windowHeight - r then -ballSpeedY
              else ballVelocityY
            )
    in
    newBallVelocity






---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container"]
        [ div []
            [ button [ onClick (GameInput Start True)] [ Html.text "Start moving!"]
            , button [ onClick (GameInput Pause True)] [ Html.text "Stop moving!"]
            , button [ onClick (GameInput Reset True)] [ Html.text "Reset the Game!"]
            ]
        , div []
            [ gameBoard model ]
        ]


gameBoard : Model -> Html Msg
gameBoard model =
    case model of
        NotPlaying _ ->
            displayGameBoard model
        Playing _ ->
            displayGameBoard model
        Won _ ->
            displayGameBoard model
        Lost _ ->
            displayGameBoard model


displayGameBoard : GameState -> Html Msg
displayGameBoard gameState =
    let
        (state, drawModel) =
            case gameState of
                NotPlaying gameModel -> (Objects.Background.NotPlaying, gameModel)
                Playing gameModel -> (Objects.Background.Playing, gameModel)
                Won gameModel -> (Objects.Background.Won, gameModel)
                Lost gameModel -> (Objects.Background.Lost, gameModel)
    in
    Svg.svg
        [ width "100%"
        , height "100%"
        , viewBox "0 0 100 77"
        ]
        ( List.append
            [ background drawModel.windowSize
            , backgroundtext state
            , ball drawModel.ballPosition drawModel.ballRadius
            , paddle drawModel.paddlePosition drawModel.paddleSize
            ]
            (allBlocks drawModel.blocks drawModel.blockSize)
        )


updateGameDisplay : Float -> GameModel -> ( Model, Cmd Msg )
updateGameDisplay dt gameModel =
    let
        ( ballPositionX, ballPositionY ) =
            gameModel.ballPosition
        ( ballVelocityX, ballVelocityY ) =
            determineVelocity gameModel
        (paddlePositionX, paddlePositionY) = gameModel.paddlePosition
        paddleVelocityX = gameModel.paddleVelocityX
        newGameModel =
            if ballPositionY >= 75 then
                Lost gameModel
            else
                Playing
                    { gameModel
                        | ballPosition =
                            ( ballPositionX + ballVelocityX / dt
                            , ballPositionY + ballVelocityY / dt
                            )
                        , ballVelocity =
                            ( ballVelocityX
                            , ballVelocityY
                            )
                        , paddlePosition = (paddlePositionX + paddleVelocityX, paddlePositionY)
                    }
    in
    ( newGameModel, Cmd.none )



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
