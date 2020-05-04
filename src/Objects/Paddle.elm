module Objects.Paddle exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)


paddle : Float -> Svg msg
paddle xPosition =
    rect
        [ x (String.fromFloat xPosition)
        , y "76.5"
        , width "6"
        , height "2"
        , fill "#bcc0dd"
        ]
        []