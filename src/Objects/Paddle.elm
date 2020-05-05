module Objects.Paddle exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)


paddle : (Float, Float) -> (Float, Float) -> Svg msg
paddle (xPosition, yPosition) paddleSize =
    rect
        [ x (String.fromFloat xPosition)
        , y (String.fromFloat yPosition)
        , width (String.fromFloat <| Tuple.first paddleSize)
        , height (String.fromFloat <| Tuple.second paddleSize)
        , fill "#ffcb0b"
        ]
        []