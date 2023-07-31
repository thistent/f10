module El.Background exposing
    ( color
    , gradient
    , image
    , tiled
    , tiledX
    , tiledY
    , uncropped
    )

import El exposing (..)
import Element.Background as B


color : Color -> Attr decorative msg
color clr =
    [ B.color clr ]


gradient :
    { angle : Float
    , steps : List Color
    }
    -> Attr decorative msg
gradient grad =
    [ B.gradient grad ]


image : String -> Attribute msg
image src =
    [ B.image src ]


tiled : String -> Attribute msg
tiled src =
    [ B.tiled src ]


tiledX : String -> Attribute msg
tiledX src =
    [ B.tiledX src ]


tiledY : String -> Attribute msg
tiledY src =
    [ B.tiledY src ]


uncropped : String -> Attribute msg
uncropped src =
    [ B.uncropped src ]
