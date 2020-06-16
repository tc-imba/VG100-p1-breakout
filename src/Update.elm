module Update exposing (..)

import Array
import Debug exposing (log)
import Keyboard exposing (RawKey)
import Model exposing (..)
import Sound exposing (playSound)


type Collision
    = Wall
    | Paddle
    | Block
    | None



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
        GameInput Start isDown ->
            case isDown of
                True ->
                    case model.state of
                        NotPlaying gameModel ->
                            let
                                newModel =
                                    { gameModel
                                        | ballMovingDirection = ( 1, -1 )
                                        , currentTime = 0
                                        , lastHitTime = 0
                                        , combo = 0
                                    }
                            in
                            ( { model | state = Playing newModel }, playSound "start" )

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
                            ( { model | state = NotPlaying newModel }, playSound "pause" )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GameInput Reset isDown ->
            case isDown of
                True ->
                    ( { model | state = NotPlaying initGameModel }, playSound "reset" )

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
                    let
                        ( state, cmd ) =
                            updateGameDisplay dt gameModel
                    in
                    ( { model | state = state }, cmd )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


determineBlockCollision : GameModel -> { direction : ( Float, Float ), judgeCollision : Bool, block : ( Int, ( Int, Int ) ) }
determineBlockCollision gameModel =
    let
        ( ballPositionX, ballPositionY ) =
            gameModel.ballPosition

        ( balldirectionX, balldirectionY ) =
            gameModel.ballMovingDirection

        r =
            gameModel.ballRadius

        ( width, height ) =
            gameModel.blockSize

        blockJudge =
            gameModel.blocks

        ( ballEdgeX, ballEdgeY ) =
            ( ballPositionX + balldirectionX * r, ballPositionY + balldirectionY * r )

        ( indexCenterX, indexCenterY ) =
            ( Basics.floor (ballPositionX / width), Basics.floor (ballPositionY / height) )

        ( indexEdgeX, indexEdgeY ) =
            ( Basics.floor (ballEdgeX / width), Basics.floor (ballEdgeY / height) )

        blockNumber =
            Tuple.second gameModel.blockNumber

        upDownLife =
            Maybe.withDefault (Array.repeat blockNumber 0) (Array.get indexCenterX blockJudge) |> Array.get indexEdgeY |> Maybe.withDefault 0

        leftRightLife =
            Maybe.withDefault (Array.repeat blockNumber 0) (Array.get indexEdgeX blockJudge) |> Array.get indexCenterY |> Maybe.withDefault 0

        cornerLife =
            Maybe.withDefault (Array.repeat blockNumber 0) (Array.get indexEdgeX blockJudge) |> Array.get indexEdgeY |> Maybe.withDefault 0
    in
    if upDownLife > 0 then
        { direction = ( balldirectionX, -balldirectionY )
        , judgeCollision = True
        , block =
            ( upDownLife
            , ( indexCenterX, indexEdgeY )
            )
        }

    else if leftRightLife > 0 then
        { direction = ( -balldirectionX, balldirectionY )
        , judgeCollision = True
        , block =
            ( leftRightLife
            , ( indexEdgeX, indexCenterY )
            )
        }

    else if cornerLife > 0 then
        { direction = ( -balldirectionX, -balldirectionY )
        , judgeCollision = True
        , block =
            ( cornerLife
            , ( indexEdgeX, indexEdgeY )
            )
        }

    else
        { direction = ( balldirectionX, balldirectionY )
        , judgeCollision = False
        , block =
            ( 0
            , ( -1, -1 )
            )
        }


determinePaddleCollision : GameModel -> { direction : ( Float, Float ), judgeCollision : Bool, newBallSpeedX : Float }
determinePaddleCollision gameModel =
    let
        ( ballPositionX, ballPositionY ) =
            gameModel.ballPosition

        ( ballDirectionX, ballDirectionY ) =
            gameModel.ballMovingDirection

        r =
            gameModel.ballRadius

        ( width, height ) =
            gameModel.paddleSize

        ( paddlePositionX, paddlePositionY ) =
            gameModel.paddlePosition

        paddleVelocity =
            gameModel.paddleVelocityX

        ballSpeedX =
            Tuple.first gameModel.ballMovingSpeed

        paddleEffective =
            gameModel.effectiveOfPaddle
    in
    if ballDirectionY >= 0 && ballPositionY + r >= paddlePositionY && ballPositionY + r <= paddlePositionY + 1 && ballPositionX >= paddlePositionX && ballPositionX <= paddlePositionX + width then
        let
            ( newBallDirectionX, newBallSpeedX ) =
                if ballSpeedX * ballDirectionX + paddleVelocity * paddleEffective >= 0 then
                    ( ballDirectionX, ballSpeedX * ballDirectionX + paddleVelocity * paddleEffective )

                else
                    ( ballDirectionX, -(ballSpeedX * ballDirectionX + paddleVelocity * paddleEffective) )
        in
        { direction = ( newBallDirectionX, -ballDirectionY )
        , judgeCollision = True
        , newBallSpeedX = newBallSpeedX
        }

    else if ballDirectionY <= 0 && ballPositionY - r <= paddlePositionY + height && ballPositionY - r > paddlePositionY + height - 1 && ballPositionX >= paddlePositionX && ballPositionX <= paddlePositionX + width then
        let
            ( newBallDirectionX, newBallSpeedX ) =
                if ballSpeedX * ballDirectionX + paddleVelocity * paddleEffective >= 0 then
                    ( ballDirectionX, ballSpeedX * ballDirectionX + paddleVelocity * paddleEffective )

                else
                    ( ballDirectionX, -(ballSpeedX * ballDirectionX + paddleVelocity * paddleEffective) )
        in
        { direction = ( newBallDirectionX, -ballDirectionY )
        , judgeCollision = True
        , newBallSpeedX = newBallSpeedX
        }
        --else if ballDirectionX > 0 && ballPositionX + r >= paddlePositionX && ballPositionX + r <= paddlePositionX + 1 && ballPositionY >= paddlePositionY && ballPositionY <= paddlePositionY + height then
        --    { direction = ( -1, ballPositionY)
        --    , judgeCollision = True
        --    , newBallSpeedX = ballSpeedX
        --    }
        --
        --else if ballDirectionX < 0 && ballPositionX - r <= paddlePositionX + width && ballPositionX - r >= paddlePositionX + width - 1 && ballPositionY >= paddlePositionY && ballPositionY <= paddlePositionY + height then
        --    { direction = ( 1, ballPositionY)
        --    , judgeCollision = True
        --    , newBallSpeedX = ballSpeedX
        --    }

    else if distance ( ballPositionX, ballPositionY ) ( paddlePositionX, paddlePositionY ) < r * r then
        { direction = ( -1, -1 )
        , judgeCollision = True
        , newBallSpeedX = ballSpeedX
        }

    else if distance ( ballPositionX, ballPositionY ) ( paddlePositionX + width, paddlePositionY ) < r * r then
        { direction = ( 1, -1 )
        , judgeCollision = True
        , newBallSpeedX = ballSpeedX
        }

    else if distance ( ballPositionX, ballPositionY ) ( paddlePositionX, paddlePositionY + height ) < r * r then
        { direction = ( -1, 1 )
        , judgeCollision = True
        , newBallSpeedX = ballSpeedX
        }

    else if distance ( ballPositionX, ballPositionY ) ( paddlePositionX + width, paddlePositionY + height ) < r * r then
        { direction = ( 1, 1 )
        , judgeCollision = True
        , newBallSpeedX = ballSpeedX
        }

    else
        { direction = ( ballDirectionX, ballDirectionY )
        , judgeCollision = False
        , newBallSpeedX = ballSpeedX
        }


distance : ( Float, Float ) -> ( Float, Float ) -> Float
distance ( x1, y1 ) ( x2, y2 ) =
    (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2)


determineWallCollision : GameModel -> { direction : ( Float, Float ), judgeCollision : Bool }
determineWallCollision gameModel =
    let
        ( ballPositionX, ballPositionY ) =
            gameModel.ballPosition

        r =
            gameModel.ballRadius

        ( ballDirectionX, ballDirectionY ) =
            gameModel.ballMovingDirection

        ( windowWidth, windowHeight ) =
            gameModel.windowSize

        ( newBallDirectionX, newBallDirectionY ) =
            ( if ballPositionX <= r then
                1

              else if ballPositionX >= windowWidth - r then
                -1

              else
                0
            , if ballPositionY <= r then
                1

              else if ballPositionY >= windowHeight - r then
                -1

              else
                0
            )
    in
    case ( newBallDirectionX, newBallDirectionY ) of
        ( 0, 0 ) ->
            { direction = ( ballDirectionX, ballDirectionY )
            , judgeCollision = False
            }

        ( 0, _ ) ->
            { direction = ( ballDirectionX, newBallDirectionY )
            , judgeCollision = True
            }

        ( _, 0 ) ->
            { direction = ( newBallDirectionX, ballDirectionY )
            , judgeCollision = True
            }

        _ ->
            { direction = ( newBallDirectionX, newBallDirectionY )
            , judgeCollision = True
            }


determineCollisionResult : GameModel -> { direction : ( Float, Float ), block : ( Int, ( Int, Int ) ), newBallSpeed : ( Float, Float ), changeSpeed : Bool, collision : Collision }
determineCollisionResult gameModel =
    let
        judgeWallCollision =
            determineWallCollision gameModel

        ballSpeed =
            gameModel.ballMovingSpeed
    in
    if judgeWallCollision.judgeCollision then
        { direction = judgeWallCollision.direction
        , block = ( 0, ( -1, -1 ) )
        , newBallSpeed = ballSpeed
        , changeSpeed = False
        , collision = Wall
        }

    else
        let
            judgePaddleCollision =
                determinePaddleCollision gameModel
        in
        if judgePaddleCollision.judgeCollision then
            { direction = judgePaddleCollision.direction
            , block = ( 0, ( -1, -1 ) )
            , newBallSpeed = ( judgePaddleCollision.newBallSpeedX, Tuple.second ballSpeed )
            , changeSpeed = True
            , collision = Paddle
            }

        else
            let
                judgeBlockCollision =
                    determineBlockCollision gameModel

                collision =
                    if judgeBlockCollision.judgeCollision then
                        Block

                    else
                        None
            in
            { direction = judgeBlockCollision.direction
            , block = judgeBlockCollision.block
            , newBallSpeed = ballSpeed
            , changeSpeed = False
            , collision = collision
            }


determineVelocity :
    GameModel
    ->
        { direction : ( Float, Float )
        , judgeCollision : Bool
        , block : ( Int, ( Int, Int ) )
        }
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
                ( if Tuple.first newBallDirection == 0 then
                    ballDirectionX

                  else
                    Tuple.first newBallDirection
                , if Tuple.second newBallDirection == 0 then
                    ballDirectionY

                  else
                    Tuple.second newBallDirection
                )
        in
        { direction = finalDirection
        , judgeCollision = False
        , block =
            ( 0
            , ( -1, -1 )
            )
        }


determineCollisionSound : Collision -> Int -> Cmd Msg
determineCollisionSound collision combo =
    case collision of
        Wall ->
            Cmd.none

        Paddle ->
            Cmd.none

        Block ->
            if combo == 0 || modBy 5 combo /= 0 then
                playSound "hit"
            else
                playSound "combo"

        None ->
            Cmd.none


updateGameDisplay : Float -> GameModel -> ( GameState, Cmd Msg )
updateGameDisplay dt gameModel =
    let
        ( ballPositionX, ballPositionY ) =
            gameModel.ballPosition

        collisionResult =
            determineCollisionResult gameModel

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

        blockNumber =
            Tuple.second gameModel.blockNumber

        oldBlocks =
            gameModel.blocks

        blockList =
            List.foldl (\x a -> List.append x a)
                []
                (Array.toList
                    (Array.map (\row -> Array.toList row) oldBlocks)
                )

        judgeWin =
            List.sum blockList == 0

        newBlocks =
            if judgeBlock > 0 then
                Array.set blockIndexX
                    (Array.set blockIndexY (judgeBlock - 1) (Maybe.withDefault (Array.repeat blockNumber 0) (Array.get blockIndexX oldBlocks)))
                    oldBlocks

            else
                oldBlocks

        newBallMovingSpeed =
            if collisionResult.changeSpeed then
                collisionResult.newBallSpeed

            else
                ( ballSpeedX, ballSpeedY )

        remainedLife =
            gameModel.life

        r =
            gameModel.ballRadius

        windowHeight =
            Tuple.second gameModel.windowSize

        currentTime =
            gameModel.currentTime + dt
    in
    if ballPositionY >= windowHeight - r && remainedLife == 0 then
        ( Lost gameModel, playSound "end" )

    else if ballPositionY >= windowHeight - r && remainedLife > 0 then
        ( Playing
            { gameModel
                | life = remainedLife - 1
                , ballMovingDirection =
                    ( ballDirectionX, -1 )
                , ballPosition =
                    ( ballPositionY, windowHeight - r - 10 )
                , currentTime = currentTime
            }
        , Cmd.none
        )

    else if judgeWin then
        ( Won gameModel, Cmd.none )

    else
        let
            ( hitCount, lastHitTime ) =
                case collisionResult.collision of
                    Block ->
                        ( 1, currentTime )

                    _ ->
                        ( 0, gameModel.lastHitTime )

            combo =
                if currentTime - gameModel.lastHitTime > 3000 then
                    hitCount

                else
                    hitCount + gameModel.combo

            collisionSound =
                determineCollisionSound collisionResult.collision combo
        in
        ( Playing
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
                , ballMovingSpeed = newBallMovingSpeed
                , currentTime = currentTime
                , lastHitTime = lastHitTime
                , combo = combo
            }
        , collisionSound
        )
