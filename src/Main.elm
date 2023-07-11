module Main exposing (..)

import Element as El exposing (Element, el)
import Element.Background as Bg
import Element.Font as Font
import Html exposing (Html)


main : Html msg
main =
    El.layout
        [ Font.color <| El.rgb 1 1 1
        , Font.size 20
        , Bg.color <| El.rgb 0.05 0.05 0.05
        ]
    <|
        El.column
            [ El.centerX
            , El.padding 40
            , El.spacing 20
            , El.height El.fill

            --, El.width <| El.px 800
            , Bg.color <| El.rgb 0 0 0
            ]
            [ el [ Font.size 40, El.centerX ] <|
                El.text "Cardano Catalyst Fund10 Proposals"
            , el
                [ Font.size 30
                , Font.color <| El.rgb 0.5 0.5 0.5
                , El.alignRight
                ]
              <|
                El.text "Ken Stanton"
            , El.paragraph
                [ El.spacing 15
                , Font.justify
                , Font.color <| El.rgb 0.65 0.85 1.0
                ]
                [ El.text "[[ Links to all my Fund10 proposals will be here ]]"
                ]
            ]
