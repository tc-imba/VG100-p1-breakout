module Objects.Blocks exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Array
import Basics


block : (Float, Float) -> (Float, Float) -> Svg msg
block (xPosition, yPosition) (blockWidth, blockHeight) =
    rect
        [ x (String.fromFloat xPosition)
        , y (String.fromFloat yPosition)
        , width (String.fromFloat blockWidth)
        , height (String.fromFloat blockHeight)
        , fill "#777777"
        , stroke "#d8dadd"
        , strokeWidth "0.1"
        ]
        []

allBlocks : (Array.Array (Array.Array Bool)) -> (Float, Float) -> List (Svg msg)
allBlocks blocks (blockWidth, blockHeight) =
    let
        boolList =
            List.foldl (\x a -> List.append x a) []
                (Array.toList
                    (Array.indexedMap
                        ( \i row ->
                            Array.toList
                                (Array.indexedMap
                                    ( \j element ->
                                        (getPosition (i, j) (blockWidth, blockHeight), element)
                                    )
                                    row)
                        )
                        blocks
                    )
                )
        blockPositionList = List.filter ( \x -> Tuple.second x ) boolList
    in
    List.map ( \x -> ( block (Tuple.first x) (blockWidth, blockHeight) ) ) blockPositionList


getPosition : (Int, Int) -> (Float, Float) -> (Float, Float)
getPosition (i, j) (blockWidth, blockHeight) =
    ( (Basics.toFloat i) * blockWidth, (Basics.toFloat j) * blockHeight )