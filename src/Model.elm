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
    , ballMovingDirection : ( Float, Float )
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
    , blocks : Array.Array (Array.Array Int)
    , life : Int
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
    , ballMovingDirection = ( 0, 0 )
    , ballRadius = 1.5
    , ballMovingSpeed = ( 0.04, 0.04 )
    , paddlePosition = ( 45, 70 )
    , paddleVelocityX = 0
    , paddleMovingSpeed = 0.1
    , paddleSize = ( 20, 2 )
    , windowSize = ( 100, 77 )
    , blockRange = ( blockRangeX, blockRangeY )
    , blockNumber = ( blockNumberX, blockNumberY )
    , blockSize = ( blockRangeX / blockNumberX, blockRangeY / blockNumberY )
    , blocks = Array.repeat blockNumberX <| Array.repeat blockNumberY 3
    , life = 5
    --, pressedKeys = []
    }


init : ( Model, Cmd Msg )
init =
    ( { state = NotPlaying initGameModel, mdc = Material.defaultModel }, Material.init Mdc )
