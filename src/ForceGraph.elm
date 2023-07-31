module ForceGraph exposing (main)

import Browser
import Browser.Events
import Color
import Force
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as Decode exposing (Decoder)
import Time
import TypedSvg as Ts
import TypedSvg.Attributes as Ta
import TypedSvg.Attributes.InPx as Tpx
import TypedSvg.Core as Tc exposing (Svg)
import TypedSvg.Types as Tt



-- Types --


type alias Model =
    { mouse : Xy
    , drag : Maybe Drag
    , graph : Graph Entity ()
    , simulation : Force.State NodeId
    , window : Size Float
    }


type Msg
    = DragStart NodeId Xy
    | DragAt Xy
    | DragEnd Xy
    | Tick Time.Posix


type alias Drag =
    { start : Xy
    , current : Xy
    , index : NodeId
    }


type alias Entity =
    Force.Entity NodeId { value : String }


type alias Size a =
    { width : a, height : a }


type alias Xy =
    { x : Float, y : Float }



-- Main --


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subs
        , view = view
        }



-- Initialization --


init : () -> ( Model, Cmd Msg )
init _ =
    let
        { width, height } =
            Size 900 550

        graph =
            Graph.mapContexts initializeNode testGraph

        link { from, to } =
            ( from, to )

        forces =
            [ Graph.edges graph
                |> List.map link
                |> Force.links
            , Graph.nodes graph
                |> List.map .id
                |> Force.manyBody
            , Force.center (width / 2) (height / 2)
            ]
    in
    ( Model (Xy 0 0) Nothing graph (Force.simulation forces) (Size 0 0), Cmd.none )



-- Update --


update : Msg -> Model -> Model
update msg ({ drag, graph, simulation } as model) =
    case msg of
        Tick t ->
            let
                ( newState, list ) =
                    Graph.nodes graph
                        |> List.map .label
                        |> Force.tick simulation
            in
            case drag of
                Nothing ->
                    Model
                        (Xy 0 0)
                        drag
                        (updateGraphWithList graph list)
                        newState
                        (Size 0 0)

                Just { current, index } ->
                    Model (Xy 0 0)
                        drag
                        (Graph.update index
                            (updateNode current |> Maybe.map)
                            (updateGraphWithList graph list)
                        )
                        newState
                        (Size 0 0)

        DragStart index current ->
            Model (Xy 0 0)
                (Just (Drag current current index))
                graph
                simulation
                (Size 0 0)

        DragAt current ->
            case drag of
                Just { start, index } ->
                    Model (Xy 0 0)
                        (Just (Drag start current index))
                        (Graph.update index
                            (updateNode current |> Maybe.map)
                            graph
                        )
                        (Force.reheat simulation)
                        (Size 0 0)

                Nothing ->
                    Model (Xy 0 0) Nothing graph simulation (Size 0 0)

        DragEnd current ->
            case drag of
                Just { start, index } ->
                    -- FIXME!
                    Model (Xy 0 0)
                        Nothing
                        (Graph.update index
                            (updateNode current |> Maybe.map)
                            graph
                        )
                        simulation
                        (Size 0 0)

                Nothing ->
                    -- FIXME!
                    Model (Xy 0 0) Nothing graph simulation (Size 0 0)



-- Subscriptions --


subs : Model -> Sub Msg
subs model =
    case model.drag of
        Nothing ->
            if Force.isCompleted model.simulation then
                Sub.none

            else
                Browser.Events.onAnimationFrame Tick

        Just _ ->
            Sub.batch
                [ Browser.Events.onMouseMove
                    (Decode.map
                        (.clientPos >> tupleToXy >> DragAt)
                        Mouse.eventDecoder
                    )
                , Browser.Events.onMouseUp
                    (Decode.map
                        (.clientPos >> tupleToXy >> DragEnd)
                        Mouse.eventDecoder
                    )
                , Browser.Events.onAnimationFrame Tick
                ]


xyToTuple : Xy -> ( Float, Float )
xyToTuple p =
    ( p.x, p.y )


tupleToXy : ( Float, Float ) -> Xy
tupleToXy ( x, y ) =
    Xy x y


emptyEntity : NodeId -> String -> Entity
emptyEntity id val =
    Force.entity id val


initializeNode : NodeContext String () -> NodeContext Entity ()
initializeNode ctx =
    { node =
        { label = Force.entity ctx.node.id ctx.node.label
        , id = ctx.node.id
        }
    , incoming = ctx.incoming
    , outgoing = ctx.outgoing
    }


updateNode : Xy -> NodeContext Entity () -> NodeContext Entity ()
updateNode pos nodeCtx =
    let
        nodeValue : Entity
        nodeValue =
            nodeCtx.node.label
    in
    updateContext
        { nodeValue | x = pos.x, y = pos.y }
        nodeCtx


updateContext :
    Entity
    -> NodeContext Entity ()
    -> NodeContext Entity ()
updateContext value ({ node } as nodeCtx) =
    { nodeCtx | node = { node | label = value } }


updateGraphWithList : Graph Entity () -> List Entity -> Graph Entity ()
updateGraphWithList =
    let
        updateEntity : Entity -> Graph Entity () -> Graph Entity ()
        updateEntity node graph =
            Graph.update node.id
                (updateContext node |> Maybe.map)
                graph
    in
    List.foldr updateEntity


onMouseDown : NodeId -> Tc.Attribute Msg
onMouseDown index =
    Mouse.onDown (.clientPos >> tupleToXy >> DragStart index)


linkElement : Graph Entity () -> Edge () -> Svg msg
linkElement graph edge =
    let
        source =
            Graph.get edge.from graph
                |> Maybe.map (.node >> .label)
                |> Maybe.withDefault (Force.entity 0 "")

        target =
            Graph.get edge.to graph
                |> Maybe.map (.node >> .label)
                |> Maybe.withDefault (Force.entity 0 "")
    in
    Ts.line
        [ Tpx.strokeWidth 1
        , Color.rgb255 0x33 0x33 0x33
            |> Tt.Paint
            |> Ta.stroke
        , Tpx.x1 source.x
        , Tpx.y1 source.y
        , Tpx.x2 target.x
        , Tpx.y2 target.y
        ]
        []


nodeElement : { a | id : NodeId, label : { b | x : Float, y : Float, value : String } } -> Svg Msg
nodeElement node =
    Ts.circle
        [ Tpx.r 2.5
        , Color.rgb255 0xFF 0x00 0x00
            |> Tt.Paint
            |> Ta.fill
        , Color.rgba 0 0 0 0
            |> Tt.Paint
            |> Ta.stroke
        , Tpx.strokeWidth 7
        , onMouseDown node.id
        , Tpx.cx node.label.x
        , Tpx.cy node.label.y
        ]
        [ Ts.title [] [ Tc.text node.label.value ] ]


view : Model -> Svg Msg
view model =
    let
        w =
            model.window.width

        h =
            model.window.height
    in
    Ts.svg [ Ta.viewBox 0 0 w h ]
        [ Ts.rect
            [ Tpx.x 0
            , Tpx.y 0
            , Tpx.width w
            , Tpx.height h
            , Color.rgb255 0x11 0x11 0x11
                |> Tt.Paint
                |> Ta.fill
            ]
            []
        , Graph.edges model.graph
            |> List.map (linkElement model.graph)
            |> Ts.g [ Ta.class [ "links" ] ]
        , Graph.nodes model.graph
            |> List.map nodeElement
            |> Ts.g [ Ta.class [ "nodes" ] ]
        ]


testGraph : Graph String ()
testGraph =
    Graph.fromNodeLabelsAndEdgePairs
        [ "Dims"
        , "Real DAOs"
        , "Mobile Money in Malawi"
        , "Content based DRep Identity"
        ]
        [ ( 0, 1 )
        , ( 0, 2 )
        , ( 0, 3 )
        , ( 0, 4 )
        , ( 1, 2 )
        , ( 1, 4 )
        , ( 2, 4 )
        ]



{- Old Stuff --

   module Main exposing (..)

   import Browser
   import Browser.Dom as Dom
   import Browser.Events as Events
   import Color as C
   import Color.Manipulate as CM
   import Delay exposing (Delay)
   import Ease
   import Element as El exposing (Color, Element, el)
   import Element.Background as Bg
   import Element.Border as Border
   import Element.Font as Font
   import Force exposing (Force)
   import Graph exposing (Graph)
   import Html exposing (Html)
   import Html.Events.Extra.Mouse as Mouse
   import Html.Events.Extra.Touch as Touch
   import Http
   import Json.Decode as Jd exposing (Decoder)
   import Task
   import TypedSvg as Ts
   import TypedSvg.Attributes as Ta
   import TypedSvg.Core as Tc exposing (Svg)
   import TypedSvg.Filters.Attributes as Tf
   import TypedSvg.Types as Tt



   type alias Model =
       { mousePos : XY
       , graph : Graph String Entity -- FIXME: NodeId?
       , drag : Maybe Drag
       , forces : Maybe (Force.State NodeId)
       }


   type Msg
       = DragStart NodeId XY
       | DragAt XY
       | DragEnd XY


   type alias XY =
       { x : Float, y : Float }


   type alias Drag =
       { start : XY
       , current : XY
       , index : NodeId
       }


   type alias NodeId =
       Int


   type alias Entity =
       Force.Entity NodeId { value : String }


   emptyEntity : NodeId -> String -> Entity
   emptyEntity id val =
       Force.entity id val



   init : ( Model, Cmd Msg )
   init =
       ( { project = emptyProject
         , window = Delay.wait waitTime Nothing
         , graph
         }
       , Cmd.batch
           [ Task.perform WindowSize Dom.getViewport
           , Http.get
               { url = "notes/dims.json"
               , expect = Http.expectJson GotDoc projectDecoder
               }
           ]
       )


   subs : Model -> Sub Msg
   subs model =
       Sub.batch
           [ Events.onAnimationFrameDelta Tick
           ]




   edges =
       { top = 0
       , bottom = 0
       , right = 0
       , left = 0
       }

-}
