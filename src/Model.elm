module Model exposing (..)

import Array
import Keyboard
import Material exposing (Msg)



---- MODEL ----


type alias Model =
    { state : GameState
    , mdc : Material.Model Msg
    }


type GameState
    = NotPlaying GameModel
    | Playing GameModel
    | Won GameModel
    | Lost GameModel


type alias GameModel =
    { ballPosition : ( Float, Float )
    , ballVelocity : ( Float, Float )
    , ballRadius : Float
    , ballMovingSpeed : ( Float, Float )
    , paddlePosition : ( Float, Float )
    , paddleVelocityX : Float
    , paddleMovingSpeed : Float
    , paddleSize : ( Float, Float )
    , windowSize : ( Float, Float )
    , blockRange : ( Float, Float )
    , blockNumber : ( Int, Int )
    , blockSize : ( Float, Float )
    , blocks : Array.Array (Array.Array Bool)

    --, pressedKeys: List Key
    }


type Msg
    = TimeUpdate Float
    | GameInput GameControl Bool
    | KeyboardMsg Keyboard.Msg
    | StartMoving
    | StopMoving
    | NoOp
    | Mdc (Material.Msg Msg)


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
    , ballVelocity = ( 0, 0 )
    , ballRadius = 2
    , ballMovingSpeed = ( 10, 10 )
    , paddlePosition = ( 45, 70 )
    , paddleVelocityX = 0
    , paddleMovingSpeed = 2
    , paddleSize = ( 20, 2 )
    , windowSize = ( 100, 77 )
    , blockRange = ( blockRangeX, blockRangeY )
    , blockNumber = ( blockNumberX, blockNumberY )
    , blockSize = ( blockRangeX / blockNumberX, blockRangeY / blockNumberY )
    , blocks = Array.repeat blockNumberX <| Array.repeat blockNumberY True

    --, pressedKeys = []
    }


init : ( Model, Cmd Msg )
init =
    ( { state = NotPlaying initGameModel, mdc = Material.defaultModel }, Material.init Mdc )
