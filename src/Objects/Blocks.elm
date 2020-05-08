module Objects.Blocks exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Array
import Basics
import Maybe


block : (Float, Float) -> (Float, Float) -> Int -> Svg msg
block (xPosition, yPosition) (blockWidth, blockHeight) life =
    let
        color =
            Array.fromList
                [ "#6dbac0"
                , "#718acc"
                , "#685bd1"]
    in
    rect
        [ x (String.fromFloat xPosition)
        , y (String.fromFloat yPosition)
        , width (String.fromFloat blockWidth)
        , height (String.fromFloat blockHeight)
        , fill (Maybe.withDefault "#ffffff" (Array.get (life - 1) color))
        , stroke "#ffffff"
        , strokeWidth "0.1"
        ]
        []

allBlocks : (Array.Array (Array.Array Int)) -> (Float, Float) -> List (Svg msg)
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
        blockPositionList = List.filter ( \x -> (Tuple.second x) > 0 ) boolList
    in
    List.map ( \x -> ( block (Tuple.first x) (blockWidth, blockHeight) (Tuple.second x) ) ) blockPositionList


getPosition : (Int, Int) -> (Float, Float) -> (Float, Float)
getPosition (i, j) (blockWidth, blockHeight) =
    ( (Basics.toFloat i) * blockWidth, (Basics.toFloat j) * blockHeight )