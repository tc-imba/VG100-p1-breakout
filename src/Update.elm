module Update exposing (..)

import Keyboard exposing (RawKey)
import Material
import Model exposing (..)
import View exposing (updateGameDisplay)



---- UPDATE ----


keyToGameControl : Bool -> RawKey -> Msg
keyToGameControl isDown key =
    case Keyboard.anyKeyOriginal key of
        Just Keyboard.ArrowLeft ->
            GameInput PaddleLeft isDown

        Just Keyboard.ArrowRight ->
            GameInput PaddleRight isDown

        Just Keyboard.Spacebar ->
            GameInput Start isDown

        Just Keyboard.Enter ->
            GameInput Reset isDown

        _ ->
            NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        --KeyboardMsg keyMsg ->
        --    case model of
        --        NotPlaying gameModel ->
        --            ( NotPlaying { gameModel | pressedKeys = Keyboard.update keyMsg gameModel.pressedKeys}
        --            , Cmd.none
        --            )
        --        _ ->
        --            (model, Cmd.none)
        Mdc msg_ ->
            Material.update Mdc msg_ model

        GameInput Start isDown ->
            case isDown of
                True ->
                    case model.state of
                        NotPlaying gameModel ->
                            let
                                newModel =
                                    { gameModel
                                        | ballVelocity =
                                            ( Tuple.first gameModel.ballMovingSpeed
                                            , -1 * Tuple.second gameModel.ballMovingSpeed
                                            )
                                    }
                            in
                            ( { model | state = Playing newModel }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GameInput Pause isDown ->
            case isDown of
                True ->
                    case model.state of
                        Playing gameModel ->
                            let
                                newModel =
                                    { gameModel | ballVelocity = ( 0, 0 ) }
                            in
                            ( { model | state = NotPlaying newModel }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GameInput Reset isDown ->
            case isDown of
                True ->
                    ( { model | state = NotPlaying initGameModel }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GameInput PaddleLeft isDown ->
            case model.state of
                Playing gameModel ->
                    let
                        newModel =
                            { gameModel
                                | paddleVelocityX =
                                    if isDown then
                                        -gameModel.paddleMovingSpeed

                                    else
                                        0
                            }
                    in
                    ( { model | state = Playing newModel }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GameInput PaddleRight isDown ->
            case model.state of
                Playing gameModel ->
                    let
                        newModel =
                            { gameModel
                                | paddleVelocityX =
                                    if isDown then
                                        gameModel.paddleMovingSpeed

                                    else
                                        0
                            }
                    in
                    ( { model | state = Playing newModel }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TimeUpdate dt ->
            case model.state of
                Playing gameModel ->
                    ( { model | state = updateGameDisplay dt gameModel }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )
