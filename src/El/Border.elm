module El.Border exposing
    ( color
    , dashed
    , dotted
    , glow
    , innerGlow
    , innerShadow
    , roundEach
    , rounded
    , shadow
    , solid
    , width
    , widthEach
    , widthXY
    )

import El exposing (..)
import Element.Border as B


color : Color -> Attr decorative msg
color clr =
    [ B.color clr ]


dashed : Attribute msg
dashed =
    [ B.dashed ]


dotted : Attribute msg
dotted =
    [ B.dotted ]


glow : Color -> Float -> Attr decorative msg
glow clr size =
    [ B.glow clr size ]


innerGlow : Color -> Float -> Attr decorative msg
innerGlow clr size =
    [ B.innerGlow clr size ]


innerShadow :
    { offset : ( Float, Float )
    , size : Float
    , blur : Float
    , color : Color
    }
    -> Attr decorative msg
innerShadow almostShade =
    [ B.innerShadow almostShade ]


roundEach :
    { topLeft : Int
    , topRight : Int
    , bottomLeft : Int
    , bottomRight : Int
    }
    -> Attribute msg
roundEach corners =
    [ B.roundEach corners ]


rounded : Int -> Attribute msg
rounded radius =
    [ B.rounded radius ]


shadow :
    { offset : ( Float, Float )
    , size : Float
    , blur : Float
    , color : Color
    }
    -> Attr decorative msg
shadow almostShade =
    [ B.shadow almostShade ]


solid : Attribute msg
solid =
    [ B.solid ]


width : Int -> Attribute msg
width v =
    [ B.width v ]


widthEach :
    { bottom : Int
    , left : Int
    , right : Int
    , top : Int
    }
    -> Attribute msg
widthEach edges =
    [ B.widthEach edges ]


widthXY : Int -> Int -> Attribute msg
widthXY x y =
    [ B.widthXY x y ]
