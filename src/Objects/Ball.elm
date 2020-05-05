module Objects.Ball exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)


ball : (Float, Float) -> Float -> Svg msg
ball (xPosition, yPosition) radius =
    circle
        [ cx (String.fromFloat xPosition)
        , cy (String.fromFloat yPosition)
        , r (String.fromFloat radius)
        , fill "#002c5a"
        ]
        []
