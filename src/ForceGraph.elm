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


type alias Xy =
    { x : Float, y : Float }


type alias Size a =
    { width : a, height : a }


type Msg
    = DragStart NodeId Xy
    | DragAt Xy
    | DragEnd Xy
    | Tick Time.Posix


type alias Model =
    { mouse : Xy
    , drag : Maybe Drag
    , graph : Graph Entity ()
    , simulation : Force.State NodeId
    , window : Size Float
    }


type alias Drag =
    { start : Xy
    , current : Xy
    , index : NodeId
    }


type alias Entity =
    Force.Entity NodeId { value : String }


emptyEntity : NodeId -> String -> Entity
emptyEntity id val =
    Force.entity id val


initializeNode : NodeContext String () -> NodeContext Entity ()
initializeNode ctx =
    { node = { label = Force.entity ctx.node.id ctx.node.label, id = ctx.node.id }
    , incoming = ctx.incoming
    , outgoing = ctx.outgoing
    }


init : () -> ( Model, Cmd Msg )
init =
    let
        { width, height } =
            Size 900 550

        graph =
            Graph.mapContexts initializeNode miserablesGraph

        link { from, to } =
            ( from, to )

        forces =
            [ Force.links <| List.map link <| Graph.edges graph
            , Force.manyBody <| List.map .id <| Graph.nodes graph
            , Force.center (width / 2) (height / 2)
            ]
    in
    ( Model graph (Force.simulation forces), Cmd.none )


updateNode : ( Float, Float ) -> NodeContext Entity () -> NodeContext Entity ()
updateNode ( x, y ) nodeCtx =
    let
        nodeValue =
            nodeCtx.node.label
    in
    updateContextWithValue nodeCtx { nodeValue | x = x, y = y }


updateContextWithValue : NodeContext Entity () -> Entity -> NodeContext Entity ()
updateContextWithValue nodeCtx value =
    let
        node =
            nodeCtx.node
    in
    { nodeCtx | node = { node | label = value } }


updateGraphWithList : Graph Entity () -> List Entity -> Graph Entity ()
updateGraphWithList =
    let
        graphUpdater value =
            Maybe.map (\ctx -> updateContextWithValue ctx value)
    in
    List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)


update : Msg -> Model -> Model
update msg ({ drag, graph, simulation } as model) =
    case msg of
        Tick t ->
            let
                ( newState, list ) =
                    Force.tick simulation <| List.map .label <| Graph.nodes graph
            in
            case drag of
                Nothing ->
                    Model drag (updateGraphWithList graph list) newState

                Just { current, index } ->
                    Model drag
                        (Graph.update index
                            (Maybe.map (updateNode current))
                            (updateGraphWithList graph list)
                        )
                        newState

        DragStart index xy ->
            Model (Just (Drag xy xy index)) graph simulation

        DragAt xy ->
            case drag of
                Just { start, index } ->
                    Model (Just (Drag start xy index))
                        (Graph.update index (Maybe.map (updateNode xy)) graph)
                        (Force.reheat simulation)

                Nothing ->
                    Model Nothing graph simulation

        DragEnd xy ->
            case drag of
                Just { start, index } ->
                    Model Nothing
                        (Graph.update index (Maybe.map (updateNode xy)) graph)
                        simulation

                Nothing ->
                    Model Nothing graph simulation


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            -- This allows us to save resources, as if the simulation is done, there is no point in subscribing
            -- to the rAF.
            if Force.isCompleted model.simulation then
                Sub.none

            else
                Browser.Events.onAnimationFrame Tick

        Just _ ->
            Sub.batch
                [ Browser.Events.onMouseMove (Decode.map (.clientPos >> DragAt) Mouse.eventDecoder)
                , Browser.Events.onMouseUp (Decode.map (.clientPos >> DragEnd) Mouse.eventDecoder)
                , Browser.Events.onAnimationFrame Tick
                ]


onMouseDown : NodeId -> Tc.Attribute Msg
onMouseDown index =
    Mouse.onDown (.clientPos >> DragStart index)


linkElement : Graph Entity () -> Edge () -> Svg msg
linkElement graph edge =
    let
        source =
            Maybe.withDefault (Force.entity 0 "") <| Maybe.map (.node >> .label) <| Graph.get edge.from graph

        target =
            Maybe.withDefault (Force.entity 0 "") <| Maybe.map (.node >> .label) <| Graph.get edge.to graph
    in
    Ts.line
        [ Tpx.strokeWidth 1
        , Ta.stroke <| Tt.Paint <| Color.rgb255 0x33 0x33 0x33
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
        , Ta.fill <| Tt.Paint <| Color.rgb255 0xFF 0x00 0x00
        , Ta.stroke <| Tt.Paint <| Color.rgba 0 0 0 0
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
            , Ta.fill <| Tt.Paint <| Color.rgb255 0x11 0x11 0x11
            ]
            []
        , Graph.edges model.graph
            |> List.map (linkElement model.graph)
            |> Ts.g [ Ta.class [ "links" ] ]
        , Graph.nodes model.graph
            |> List.map nodeElement
            |> Ts.g [ Ta.class [ "nodes" ] ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }


miserablesGraph : Graph String ()
miserablesGraph =
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
