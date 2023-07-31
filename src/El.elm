module El exposing
    ( Attr
    , Attribute
    , Color
    , Column
    , Decoration
    , Device
    , DeviceClass
    , Element
    , FocusStyle
    , IndexedColumn
    , Length
    , Option
    , Orientation
    , above
    , alignBottom
    , alignLeft
    , alignRight
    , alignTop
    , alpha
    , batch
    , behindContent
    , below
    , centerX
    , centerY
    , classifyDevice
    , clip
    , clipX
    , clipY
    , column
    , download
    , downloadAs
    , el
    , explain
    , fill
    , fillPortion
    , fix
    , focusStyle
    , focused
    , forceHover
    , fromRgb
    , fromRgb255
    , height
    , html
    , htmlAttribute
    , image
    , inFront
    , indexedTable
    , layout
    , layoutWith
    , link
    , map
    , mapAttribute
    , maximum
    , minimum
    , modular
    , mouseDown
    , mouseOver
    , moveDown
    , moveLeft
    , moveRight
    , moveUp
    , newTabLink
    , noHover
    , noStaticStyleSheet
    , none
    , onLeft
    , onRight
    , padding
    , paddingEach
    , paddingXY
    , paragraph
    , pointer
    , px
    , rgb
    , rgb255
    , rgba
    , rotate
    , row
    , scale
    , scrollbarX
    , scrollbarY
    , scrollbars
    , shrink
    , spaceEvenly
    , spacing
    , spacingXY
    , table
    , text
    , textColumn
    , toRgb
    , transparent
    , width
    , wrappedRow
    )

import Element as E
import Element.Background as Bg
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as Attr


type alias Attr decorative msg =
    List (E.Attr decorative msg)


type alias Attribute msg =
    List (E.Attr () msg)


type alias Color =
    E.Color


type alias Column record msg =
    E.Column record msg


type alias Decoration =
    E.Decoration


type alias Device =
    E.Device


type alias DeviceClass =
    E.DeviceClass


type alias Element msg =
    E.Element msg


type alias FocusStyle =
    E.FocusStyle


type alias IndexedColumn record msg =
    E.IndexedColumn record msg


type alias Length =
    E.Length


type alias Option =
    E.Option


type alias Orientation =
    E.Orientation


above : Element msg -> Attribute msg
above element =
    [ E.above element ]


alignBottom : Attribute msg
alignBottom =
    [ E.alignBottom ]


alignLeft : Attribute msg
alignLeft =
    [ E.alignLeft ]


alignRight : Attribute msg
alignRight =
    [ E.alignRight ]


alignTop : Attribute msg
alignTop =
    [ E.alignTop ]


alpha : Float -> Attr decorative msg
alpha o =
    [ E.alpha o ]


batch : List (Attribute msg) -> Attribute msg
batch =
    List.concat


fix : List (E.Attr decorative msg) -> Attr decorative msg
fix l =
    l


behindContent : Element msg -> Attribute msg
behindContent element =
    [ E.behindContent element ]


below : Element msg -> Attribute msg
below element =
    [ E.below element ]


centerX : Attribute msg
centerX =
    [ E.centerX ]


centerY : Attribute msg
centerY =
    [ E.centerY ]


classifyDevice : { window | height : Int, width : Int } -> Device
classifyDevice =
    E.classifyDevice


clip : Attribute msg
clip =
    [ E.clip ]


clipX : Attribute msg
clipX =
    [ E.clipX ]


clipY : Attribute msg
clipY =
    [ E.clipY ]


column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs =
    E.column (batch attrs)


download :
    List (Attribute msg)
    ->
        { url : String
        , label : Element msg
        }
    -> Element msg
download attrs =
    E.download (batch attrs)


downloadAs :
    List (Attribute msg)
    ->
        { label : Element msg
        , filename : String
        , url : String
        }
    -> Element msg
downloadAs attrs =
    E.downloadAs (batch attrs)


el : List (Attribute msg) -> Element msg -> Element msg
el attrs =
    E.el (batch attrs)


explain : Todo -> Attribute msg
explain todo =
    [ E.explain todo ]


fill : Length
fill =
    E.fill


fillPortion : Int -> Length
fillPortion =
    E.fillPortion


focusStyle : FocusStyle -> Option
focusStyle =
    E.focusStyle


focused : List Decoration -> Attribute msg
focused decs =
    [ E.focused decs ]


forceHover : Option
forceHover =
    E.forceHover


fromRgb :
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }
    -> Color
fromRgb =
    E.fromRgb


fromRgb255 :
    { red : Int
    , green : Int
    , blue : Int
    , alpha : Float
    }
    -> Color
fromRgb255 =
    E.fromRgb255


height : Length -> Attribute msg
height l =
    [ E.height l ]


html : Html msg -> Element msg
html =
    E.html


htmlAttribute : Html.Attribute msg -> Attribute msg
htmlAttribute attr =
    [ E.htmlAttribute attr ]


image : List (Attribute msg) -> { src : String, description : String } -> Element msg
image attrs =
    E.image (batch attrs)


inFront : Element msg -> Attribute msg
inFront element =
    [ E.inFront element ]


indexedTable :
    List (Attribute msg)
    ->
        { data : List records
        , columns : List (IndexedColumn records msg)
        }
    -> Element msg
indexedTable attrs =
    E.indexedTable (batch attrs)


layout : List (Attribute msg) -> Element msg -> Html msg
layout =
    layoutWith { options = [] }


layoutWith : { options : List Option } -> List (Attribute msg) -> Element msg -> Html msg
layoutWith { options } attrs =
    E.layoutWith { options = options } (batch attrs)


link :
    List (Attribute msg)
    ->
        { url : String
        , label : Element msg
        }
    -> Element msg
link attrs =
    E.link (batch attrs)


map : (msg -> msg1) -> Element msg -> Element msg1
map =
    E.map


mapAttribute : (msg -> msg1) -> Attribute msg -> Attribute msg1
mapAttribute f =
    List.map (E.mapAttribute f)


maximum : Int -> Length -> Length
maximum =
    E.maximum


minimum : Int -> Length -> Length
minimum =
    E.minimum


modular : Float -> Float -> Int -> Float
modular =
    E.modular


mouseDown : List Decoration -> Attribute msg
mouseDown decs =
    [ E.mouseDown decs ]


mouseOver : List Decoration -> Attribute msg
mouseOver decs =
    [ E.mouseOver decs ]


moveDown : Float -> Attr decorative msg
moveDown y =
    [ E.moveDown y ]


moveLeft : Float -> Attr decorative msg
moveLeft x =
    [ E.moveRight x ]


moveRight : Float -> Attr decorative msg
moveRight x =
    [ E.moveRight x ]


moveUp : Float -> Attr decorative msg
moveUp y =
    [ E.moveUp y ]


newTabLink :
    List (Attribute msg)
    ->
        { url : String
        , label : Element msg
        }
    -> Element msg
newTabLink attrs =
    E.newTabLink (batch attrs)


noHover : Option
noHover =
    E.noHover


noStaticStyleSheet : Option
noStaticStyleSheet =
    E.noStaticStyleSheet


none : Element msg
none =
    E.none


onLeft : Element msg -> Attribute msg
onLeft element =
    [ E.onLeft element ]


onRight : Element msg -> Attribute msg
onRight element =
    [ E.onRight element ]


padding : Int -> Attribute msg
padding x =
    [ E.padding x ]


paddingEach : { top : Int, right : Int, bottom : Int, left : Int } -> Attribute msg
paddingEach edges =
    [ E.paddingEach edges ]


paddingXY : Int -> Int -> Attribute msg
paddingXY x y =
    [ E.paddingXY x y ]


paragraph : List (Attribute msg) -> List (Element msg) -> Element msg
paragraph attrs =
    E.paragraph (batch attrs)


pointer : Attribute msg
pointer =
    [ E.pointer ]


px : Int -> Length
px =
    E.px


rgb : Float -> Float -> Float -> Color
rgb =
    E.rgb


rgb255 : Int -> Int -> Int -> Color
rgb255 =
    E.rgb255


rgba : Float -> Float -> Float -> Float -> Color
rgba =
    E.rgba


rotate : Float -> Attr decorative msg
rotate angle =
    [ E.rotate angle ]


row : List (Attribute msg) -> List (Element msg) -> Element msg
row attrs =
    E.row (batch attrs)


scale : Float -> Attr decorative msg
scale n =
    [ E.scale n ]


scrollbarX : Attribute msg
scrollbarX =
    [ E.scrollbarX ]


scrollbarY : Attribute msg
scrollbarY =
    [ E.scrollbarY ]


scrollbars : Attribute msg
scrollbars =
    [ E.scrollbars ]


shrink : Length
shrink =
    E.shrink


spaceEvenly : Attribute msg
spaceEvenly =
    [ E.spaceEvenly ]


spacing : Int -> Attribute msg
spacing x =
    [ E.spacing x ]


spacingXY : Int -> Int -> Attribute msg
spacingXY x y =
    [ E.spacingXY x y ]


table :
    List (Attribute msg)
    ->
        { data : List records
        , columns : List (Column records msg)
        }
    -> Element msg
table attrs =
    E.table (batch attrs)


text : String -> Element msg
text =
    E.text


textColumn : List (Attribute msg) -> List (Element msg) -> Element msg
textColumn attrs =
    E.textColumn (batch attrs)


toRgb :
    Color
    ->
        { red : Float
        , green : Float
        , blue : Float
        , alpha : Float
        }
toRgb =
    E.toRgb


transparent : Bool -> Attr decorative msg
transparent on =
    [ E.transparent on ]


width : Length -> Attribute msg
width l =
    [ E.width l ]


wrappedRow : List (Attribute msg) -> List (Element msg) -> Element msg
wrappedRow attrs =
    E.wrappedRow (batch attrs)


{-| This is just an alias for `Debug.todo`
-}
type alias Todo =
    String -> Never



{- TODO: This stuff should go somewhere else!

   - FIXME: the alias for `Attribute msg` in this library
     is a `List (Attribute msg)` in elm-ui.

    type alias InputBoxRecord msg =
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : Input.Label msg
        , spellcheck : Bool
        }


    inputBox : List (Attribute msg) -> InputBoxRecord msg -> Element msg
    inputBox attrs =
        Input.multiline (batch attrs)


    -- Styles --


    fillSpace : List (Attribute msg)
    fillSpace =
        [ E.width E.fill, E.height E.fill ]


    centerInSpace : List (Attribute msg)
    centerInSpace =
        [ E.centerX, E.centerY ]



    -- Styles Attributes --


    fg : E.Color -> List (Attribute msg)
    fg c =
        [ Font.color c ]


    bg : E.Color -> List (Attribute msg)
    bg c =
        [ Bg.color c ]


    style : String -> String -> Attribute msg
    style s t =
        E.htmlAttribute <| Attr.style s t


    wrapAnywhere : List (Attribute msg)
    wrapAnywhere =
        --style "word-break" "break-all"
        --[ style "line-break" "auto" ]
        [ style "line-break" "anywhere" ]


    preformatted : List (Attribute msg)
    preformatted =
        [ style "white-space" "pre-line" ]


    noSelect : Bool -> List (Attribute msg)
    noSelect b =
        let
            none =
                if b then
                    "none"

                else
                    "auto"
        in
        [ style "-webkit-user-select" none
        , style "-khtml-user-select" none
        , style "-webkit-touch-callouT" none
        , style "-moz-user-select" none
        , style "-o-user-select" none
        , style "user-select" none
        ]



    -- Helpful Stuff --


    edges : { top : Int, bottom : Int, left : Int, right : Int }
    edges =
        { top = 0
        , bottom = 0
        , left = 0
        , right = 0
        }



    -- Not Important --


    yellowOnRed : List (Attribute msg)
    yellowOnRed =
        [ Font.color <| E.rgb 1 1 0
        , Bg.color <| E.rgb 0.5 0 0
        ]

-}
