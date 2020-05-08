module Update exposing (..)

import Keyboard exposing (RawKey)
import Material
import Model exposing (..)
import Array
import Debug exposing (log)


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
                                        | ballMovingDirection =
                                            ( 1, -1 )
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
                                    { gameModel | ballMovingDirection = ( 0, 0 ) }
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


determineBlockCollision : GameModel -> { direction: ( Float, Float), block: ( Bool, (Int, Int) ) }
determineBlockCollision gameModel =
    let
        ( ballPositionX, ballPositionY ) = gameModel.ballPosition

        ( balldirectionX, balldirectionY ) = gameModel.ballMovingDirection

        r = gameModel.ballRadius

        ( width, height ) = gameModel.blockSize

        blockJudge = gameModel.blocks

        ( ballEdgeX, ballEdgeY ) = ( ballPositionX + balldirectionX * r, ballPositionY + balldirectionY * r )

        ( indexCenterX, indexCenterY ) = ( Basics.floor (ballPositionX / width), Basics.floor (ballPositionY / height) )

        ( indexEdgeX, indexEdgeY ) = ( Basics.floor (ballEdgeX / width), Basics.floor (ballEdgeY / height) )

        blockNumber = Tuple.second gameModel.blockNumber
    in
    if (Maybe.withDefault (Array.repeat blockNumber False) (Array.get indexCenterX blockJudge)) |> Array.get indexEdgeY |> Maybe.withDefault False then
        { direction = ( balldirectionX, -balldirectionY )
        , block =
            ( True
            , ( indexCenterX, indexEdgeY )
            )
        }
    else if (Maybe.withDefault (Array.repeat blockNumber False) (Array.get indexEdgeX blockJudge)) |> Array.get indexCenterY |> Maybe.withDefault False then
        { direction = ( -balldirectionX, balldirectionY )
        , block =
            ( True
            , ( indexEdgeX, indexCenterY )
            )
        }
    else if (Maybe.withDefault (Array.repeat blockNumber False) (Array.get indexEdgeX blockJudge)) |> Array.get indexEdgeY |> Maybe.withDefault False then
        { direction = ( -balldirectionX, -balldirectionY )
        , block =
            ( True
            , ( indexEdgeX, indexEdgeY )
            )
        }
    else
        { direction = ( balldirectionX, balldirectionY )
        , block =
            ( False
            , ( -1, -1 )
            )
        }



determineVelocity : GameModel -> { direction: ( Float, Float), block: ( Bool, (Int, Int) ) }
determineVelocity gameModel =
    let
        ( ballPositionX, ballPositionY ) =
            gameModel.ballPosition

        ( ballDirectionX, ballDirectionY ) =
            gameModel.ballMovingDirection

        ( paddlePositionX, paddlePositionY ) =
            gameModel.paddlePosition

        ( paddleWidth, paddleHeight ) =
            gameModel.paddleSize

        r =
            gameModel.ballRadius

        ( windowWidth, windowHeight ) =
            gameModel.windowSize

        --judgeBlockRange = ((Tuple.second gameModel.blockRange + r) >= ballPositionY)

        newBallDirection =
            ( if ballPositionX <= r then
                1

              else if ballPositionX >= windowWidth - r then
                -1

              else if ballPositionX >= paddlePositionX && ballPositionX <= paddlePositionX + 0.5 && ballPositionY >= paddlePositionY && ballPositionY <= paddlePositionY + paddleHeight then
                -1

              else if ballPositionX <= paddlePositionX + paddleWidth && ballPositionX >= paddlePositionX + paddleWidth - 0.5 && ballPositionY >= paddlePositionY && ballPositionY <= paddlePositionY + paddleHeight then
                1

              else
                0
            , if ballPositionY <= r then
                1

              else if ballPositionY >= paddlePositionY - r && ballPositionY <= paddlePositionY - r + 1 && ballPositionX >= paddlePositionX && ballPositionX <= paddlePositionX + paddleWidth then
                -1

              else if ballPositionY >= windowHeight - r then
                -1

              else
                0
            )
    in
        if Tuple.first newBallDirection == 0 && Tuple.second newBallDirection == 0 then
            determineBlockCollision gameModel
        else
            let
                finalDirection =
                    ( if Tuple.first newBallDirection == 0 then ballDirectionX
                      else Tuple.first newBallDirection
                    , if Tuple.second newBallDirection == 0 then ballDirectionY
                      else Tuple.second newBallDirection
                    )
            in
            { direction = finalDirection
            , block =
                ( False
                , ( -1, -1 )
                )
            }


updateGameDisplay : Float -> GameModel -> GameState
updateGameDisplay dt gameModel =
    let
        ( ballPositionX, ballPositionY ) =
            gameModel.ballPosition

        collisionResult = determineVelocity gameModel

        ( ballDirectionX, ballDirectionY ) =
            collisionResult.direction

        ( judgeBlock, ( blockIndexX, blockIndexY ) ) =
            collisionResult.block

        ( ballSpeedX, ballSpeedY ) =
            gameModel.ballMovingSpeed

        ( paddlePositionX, paddlePositionY ) =
            gameModel.paddlePosition

        paddleVelocityX =
            gameModel.paddleVelocityX

        blockNumber = Tuple.second gameModel.blockNumber

        oldBlocks = gameModel.blocks

        blockList =
            List.foldl (\x a -> List.append x a) []
                ( Array.toList
                    (Array.map (\row -> Array.toList row) oldBlocks)
                )

        judgeNotWin = List.member True blockList

        newBlocks =
            if judgeBlock then
                Array.set blockIndexX
                    (Array.set blockIndexY False (Maybe.withDefault (Array.repeat blockNumber False) (Array.get blockIndexX oldBlocks)))
                    oldBlocks
            else
                oldBlocks
    in
    if ballPositionY >= 75 then
        Lost gameModel

    else if not judgeNotWin then
        Won gameModel

    else
        Playing
            { gameModel
                | ballPosition =
                    ( ballPositionX + ballDirectionX * ballSpeedX * dt
                    , ballPositionY + ballDirectionY * ballSpeedY * dt
                    )
                , ballMovingDirection =
                    ( ballDirectionX
                    , ballDirectionY
                    )
                , paddlePosition = ( paddlePositionX + paddleVelocityX * dt, paddlePositionY )
                , blocks = newBlocks
            }

