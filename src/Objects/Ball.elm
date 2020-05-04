module Objects.Ball exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)


ball : (Float, Float) -> Svg msg
ball (xPosition, yPosition) =
    circle
        [ cx (String.fromFloat xPosition)
        , cy (String.fromFloat yPosition)
        , r "2"
        , fill "#002c5a"
        ]
        []
