module Objects.Background exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)


type StateType
    = NotPlaying
    | Playing
    | Won
    | Lost

backgroundtext : StateType -> Svg msg
backgroundtext gameState =
    let
        text =
            case gameState of
                NotPlaying -> "Press Space to Start."
                Playing -> ""
                Won -> "Congratulations! You won! Press Enter to play again."
                Lost -> "Game over! Press Enter to try again."
    in
    Svg.text_
        [ pointerEvents "none" -- prevents typing cursor (and mousedown-capture, though this is behind all other objects so that doesn't matter)
        , x         "100"
        , y         "30"
        , fontSize  "5"
        , textAnchor "middle"
        , fill "#002c5a"
        ]
        [ Svg.tspan [x "50", dy "1.2em"] [Svg.text text]
        ]

background : (Float, Float) -> Svg msg
background (boxWidth, boxHeight) =
    rect
        [ x (String.fromFloat 0)
        , y (String.fromFloat 0)
        , width (String.fromFloat boxWidth)
        , height (String.fromFloat boxHeight)
        , fill "#d8dadd"
        ]
        []