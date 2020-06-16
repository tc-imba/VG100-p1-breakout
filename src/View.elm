module View exposing (..)

import Html exposing (Html, button, div)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Model exposing (..)
import Objects.Background exposing (background, backgroundtext)
import Objects.Ball exposing (ball)
import Objects.Blocks exposing (allBlocks)
import Objects.Paddle exposing (paddle)
import Svg
import Svg.Attributes exposing (height, viewBox, width)



---- VIEW ----


viewCombo : Model -> String
viewCombo model =
    case model.state of
        Playing m ->
            "Combo: " ++ String.fromInt m.combo

        _ ->
            "Good luck!"


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "title" ] [ Html.text (viewCombo model) ]
        , div []
            [ displayGameBoard model ]

        --, div []
        --    [ Button.view Mdc
        --        "button-start"
        --        model.mdc
        --        [ Button.ripple, Options.onClick (GameInput Start True) ]
        --        [ Html.text "Start moving!" ]
        --    , Button.view Mdc
        --                    "button-stop"
        --                    model.mdc
        --                    [ Button.ripple, Options.onClick (GameInput Pause True) ]
        --                    [ Html.text "Stop moving!" ]
        --    , Button.view Mdc
        --                    "button-reset"
        --                    model.mdc
        --                    [ Button.ripple, Options.onClick (GameInput Reset True) ]
        --                    [ Html.text "Reset the Game!" ]
        --    --, button [ onClick (GameInput Start True) ] [ Html.text "Start moving!" ]
        --    --, button [ onClick (GameInput Pause True) ] [ Html.text "Stop moving!" ]
        --    --, button [ onClick (GameInput Reset True) ] [ Html.text "Reset the Game!" ]
        --    ]
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
