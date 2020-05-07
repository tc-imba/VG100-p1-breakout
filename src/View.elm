module View exposing (..)

import Html exposing (Html, button, div)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Material.Button as Button
import Material.Options as Options
import Model exposing (..)
import Objects.Background exposing (background, backgroundtext)
import Objects.Ball exposing (ball)
import Objects.Blocks exposing (allBlocks)
import Objects.Paddle exposing (paddle)
import Svg
import Svg.Attributes exposing (height, viewBox, width)



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div []
            [ button [ onClick (GameInput Start True) ] [ Html.text "Start moving!" ]
            , button [ onClick (GameInput Pause True) ] [ Html.text "Stop moving!" ]
            , button [ onClick (GameInput Reset True) ] [ Html.text "Reset the Game!" ]
            , Button.view Mdc
                "button-start"
                model.mdc
                [ Button.ripple, Options.onClick (GameInput Start True) ]
                [ Html.text "Start moving!" ]
            ]
        , div []
            [ displayGameBoard model ]
        ]


displayGameBoard : Model -> Html Msg
displayGameBoard model =
    let
        gameState =
            model.state

        ( state, drawModel ) =
            case gameState of
                NotPlaying gameModel ->
                    ( Objects.Background.NotPlaying, gameModel )

                Playing gameModel ->
                    ( Objects.Background.Playing, gameModel )

                Won gameModel ->
                    ( Objects.Background.Won, gameModel )

                Lost gameModel ->
                    ( Objects.Background.Lost, gameModel )
    in
    Svg.svg
        [ width "100%"
        , height "100%"
        , viewBox "0 0 100 77"
        ]
        (List.append
            [ background drawModel.windowSize
            , backgroundtext state
            , ball drawModel.ballPosition drawModel.ballRadius
            , paddle drawModel.paddlePosition drawModel.paddleSize
            ]
            (allBlocks drawModel.blocks drawModel.blockSize)
        )


determineVelocity : GameModel -> ( Float, Float )
determineVelocity gameModel =
    let
        ( ballPositionX, ballPositionY ) =
            gameModel.ballPosition

        ( ballSpeedX, ballSpeedY ) =
            gameModel.ballMovingSpeed

        ( ballVelocityX, ballVelocityY ) =
            gameModel.ballVelocity

        ( paddlePositionX, paddlePositionY ) =
            gameModel.paddlePosition

        ( paddleWidth, paddleHeight ) =
            gameModel.paddleSize

        r =
            gameModel.ballRadius

        ( windowWidth, windowHeight ) =
            gameModel.windowSize

        newBallVelocity =
            ( if ballPositionX <= r then
                ballSpeedX

              else if ballPositionX >= windowWidth - r then
                -ballSpeedX

              else if ballPositionX >= paddlePositionX && ballPositionX <= paddlePositionX + 0.5 && ballPositionY >= paddlePositionY && ballPositionY <= paddlePositionY + paddleHeight then
                -ballSpeedX

              else if ballPositionX <= paddlePositionX + paddleWidth && ballPositionX >= paddlePositionX + paddleWidth - 0.5 && ballPositionY >= paddlePositionY && ballPositionY <= paddlePositionY + paddleHeight then
                ballSpeedX

              else
                ballVelocityX
            , if ballPositionY <= r then
                ballSpeedY

              else if ballPositionY >= paddlePositionY - r && ballPositionY <= paddlePositionY - r + 1 && ballPositionX >= paddlePositionX && ballPositionX <= paddlePositionX + paddleWidth then
                -ballSpeedY

              else if ballPositionY >= windowHeight - r then
                -ballSpeedY

              else
                ballVelocityY
            )
    in
    newBallVelocity


updateGameDisplay : Float -> GameModel -> GameState
updateGameDisplay dt gameModel =
    let
        ( ballPositionX, ballPositionY ) =
            gameModel.ballPosition

        ( ballVelocityX, ballVelocityY ) =
            determineVelocity gameModel

        ( paddlePositionX, paddlePositionY ) =
            gameModel.paddlePosition

        paddleVelocityX =
            gameModel.paddleVelocityX

        newGameModel =
            if ballPositionY >= 75 then
                Lost gameModel

            else
                Playing
                    { gameModel
                        | ballPosition =
                            ( ballPositionX + ballVelocityX / dt
                            , ballPositionY + ballVelocityY / dt
                            )
                        , ballVelocity =
                            ( ballVelocityX
                            , ballVelocityY
                            )
                        , paddlePosition = ( paddlePositionX + paddleVelocityX, paddlePositionY )
                    }
    in
    newGameModel
