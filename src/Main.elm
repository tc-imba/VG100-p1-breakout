module Main exposing (..)

import Browser
import Browser.Events
import Keyboard exposing (Key(..), RawKey)
import Model exposing (..)
import Update exposing (keyToGameControl, update)
import View exposing (view)



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta TimeUpdate
        , Keyboard.downs <| keyToGameControl True
        , Keyboard.ups <| keyToGameControl False
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
