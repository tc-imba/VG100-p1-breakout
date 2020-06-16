module Model exposing (..)

import Array
import Keyboard
import Time exposing (now, posixToMillis)


---- MODEL ----


type alias Model =
    { state : GameState
    }


type GameState
    = NotPlaying GameModel
    | Playing GameModel
    | Won GameModel
    | Lost GameModel


type alias GameModel =
    { ballPosition : ( Float, Float )
    , ballMovingDirection : ( Float, Float )
    , ballRadius : Float
    , ballMovingSpeed : ( Float, Float )
    , paddlePosition : ( Float, Float )
    , paddleVelocityX : Float
    , paddleMovingSpeed : Float
    , paddleSize : ( Float, Float )
    , effectiveOfPaddle : Float
    , windowSize : ( Float, Float )
    , blockRange : ( Float, Float )
    , blockNumber : ( Int, Int )
    , blockSize : ( Float, Float )
    , blocks : Array.Array (Array.Array Int)
    , life : Int
    --, pressedKeys: List Key
    , combo: Int
    , currentTime: Float
    , lastHitTime: Float
    }


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


initGameModel : GameModel
initGameModel =
    let
        blockRangeX =
            100

        blockRangeY =
            20

        blockNumberX =
            10

        blockNumberY =
            5
    in
    { ballPosition = ( 25, 70 )
    , ballMovingDirection = ( 0, 0 )
    , ballRadius = 1.5
    , ballMovingSpeed = ( 0.04, 0.04 )
    , paddlePosition = ( 45, 70 )
    , paddleVelocityX = 0
    , paddleMovingSpeed = 0.1
    , paddleSize = ( 20, 2 )
    , effectiveOfPaddle = 0.5
    , windowSize = ( 100, 77 )
    , blockRange = ( blockRangeX, blockRangeY )
    , blockNumber = ( blockNumberX, blockNumberY )
    , blockSize = ( blockRangeX / blockNumberX, blockRangeY / blockNumberY )
    , blocks = Array.repeat blockNumberX <| Array.repeat blockNumberY 3
    , life = 0
    --, pressedKeys = []
    , combo = 0
    , currentTime = 0
    , lastHitTime = 0
    }


init : ( Model, Cmd Msg )
init =
    ( { state = NotPlaying initGameModel }, Cmd.none )
