module Pane exposing (Pane, Size, SplitRenderer, hSplit, render, single, splitAbove, splitBelow, splitLeft, splitRight, vSplit)


type alias Size =
    { width : Float, height : Float }


type alias SplitRenderer a =
    { h : Float -> Size -> a -> a -> a
    , v : Float -> Size -> a -> a -> a
    }


type Pane a
    = HSplit Float (Pane a) (Pane a)
    | VSplit Float (Pane a) (Pane a)
    | RenderPane (Size -> a)


render : SplitRenderer a -> Size -> Pane a -> a
render spr size pane =
    case pane of
        RenderPane fun ->
            fun size

        HSplit f p1 p2 ->
            let
                ratio =
                    toRatio f
            in
            spr.h ratio
                size
                (render spr
                    { size | height = size.height * ratio }
                    p1
                )
                (render spr
                    { size | height = size.height * (1.0 - ratio) }
                    p2
                )

        VSplit f p1 p2 ->
            let
                ratio =
                    toRatio f
            in
            spr.v ratio
                size
                (render spr
                    { size | width = size.width * ratio }
                    p1
                )
                (render spr
                    { size | width = size.width * (1.0 - ratio) }
                    p2
                )


toRatio : Float -> Float
toRatio =
    clamp 0.05 0.95


single : (Size -> a) -> Pane a
single vFun =
    RenderPane vFun


hSplit : Float -> Pane a -> Pane a -> Pane a
hSplit ratio p1 p2 =
    HSplit ratio p1 p2


vSplit : Float -> Pane a -> Pane a -> Pane a
vSplit ratio p1 p2 =
    VSplit ratio p1 p2


splitLeft : Float -> (Size -> a) -> Pane a -> Pane a
splitLeft ratio newPaneView oldPane =
    VSplit ratio (RenderPane newPaneView) oldPane


splitRight : Float -> (Size -> a) -> Pane a -> Pane a
splitRight ratio newPaneView oldPane =
    VSplit (1.0 - ratio) oldPane (RenderPane newPaneView)


splitAbove : Float -> (Size -> a) -> Pane a -> Pane a
splitAbove ratio newPaneView oldPane =
    HSplit ratio oldPane (RenderPane newPaneView)


splitBelow : Float -> (Size -> a) -> Pane a -> Pane a
splitBelow ratio newPaneView oldPane =
    HSplit (1.0 - ratio) (RenderPane newPaneView) oldPane
