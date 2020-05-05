module Objects.Blocks exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Array
import Basics


block : (Float, Float) -> (Float, Float) -> Bool -> Svg msg
block (xPosition, yPosition) (blockWidth, blockHeight) judge =
    let
        color = if judge then "#777777" else "#777777"
    in
    rect
        [ x (String.fromFloat xPosition)
        , y (String.fromFloat yPosition)
        , width (String.fromFloat blockWidth)
        , height (String.fromFloat blockHeight)
        , fill color
        , stroke "#d8dadd"
        , strokeWidth "0.1"
        ]
        []

allBlocks : (Array.Array (Array.Array Bool)) -> (Float, Float) -> List (Svg msg)
allBlocks blocks (blockWidth, blockHeight) =
    List.foldl (\x a -> List.append x a) []
        (Array.toList
            (Array.indexedMap
                ( \i row ->
                    Array.toList
                        (Array.indexedMap
                            ( \j element ->
                                block (getPosition (i, j) (blockWidth, blockHeight)) (blockWidth, blockHeight) element
                            )
                            row)
                )
                blocks
            )
        )


getPosition : (Int, Int) -> (Float, Float) -> (Float, Float)
getPosition (i, j) (blockWidth, blockHeight) =
    ( (Basics.toFloat i) * blockWidth, (Basics.toFloat j) * blockHeight )