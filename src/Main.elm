module Main exposing (..)

import Browser
import Element as El exposing (Element, el)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Http
import Json.Decode as Jd exposing (Decoder)


type alias Model =
    { project : Project
    }


type alias Project =
    { title : String
    , link : String
    , ada : String
    , problem : String
    , solution : String
    }


emptyProject : Project
emptyProject =
    Project "Dummy Proj." "" "" "" ""


type Msg
    = GotText (Result Http.Error Project)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subs
        , view =
            \model ->
                { title = "F10 Proposals - Ken Stanton"
                , body = [ view model ]
                }
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( { project = emptyProject }
    , Http.get
        -- FIXME : This should work both locally and on github!
        -- { url = "/docs/notes/test.md?raw=1"
        --{ url = "https://raw.githubusercontent.com/thistent/f10/main/docs/notes/test.md"
        { url = "notes/test.json"
        , expect = Http.expectJson GotText projectDecoder
        }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText res ->
            case res of
                Ok p ->
                    ( { model | project = p }, Cmd.none )

                Err e ->
                    ( model, Cmd.none )


subs : Model -> Sub Msg
subs model =
    Sub.none


view : Model -> Html Msg
view model =
    El.layout
        [ Font.color <| El.rgb 1 1 1
        , Font.size 20
        , Bg.color <| El.rgb 0 0 0
        , El.width El.fill
        ]
    <|
        El.row
            [ El.width El.fill
            , El.height El.fill
            ]
            [ el [ El.width <| El.fillPortion 1 ] <| El.text ""
            , El.column
                [ El.centerX
                , El.padding 40
                , El.spacing 40
                , El.width <| El.fillPortion 6
                , El.height El.fill
                , Bg.color <| El.rgb 0.05 0.05 0.05
                ]
                [ El.row
                    [ Font.size 40
                    , Font.color <| El.rgb 0.85 1.0 0.65
                    ]
                    [ dims 40.0
                    , El.text " : Distributed Idea Management System"
                    ]
                , El.paragraph [ Font.justify, El.spacing 10 ]
                    [ El.text "This page serves two purposes. It will act as a landing page for all my projects in Cardano Catalyst Fund10, as well as being the first stage (a web-based interface) of my  "
                    , dims 20.0
                    , El.text "  project."
                    ]
                , el
                    [ Font.size 30
                    , Font.color <| El.rgb 0.5 0.5 0.5
                    , El.alignRight
                    ]
                  <|
                    El.text "Ken Stanton"
                , el [ Font.bold, Font.size 25 ] <|
                    El.text "My Fund10 Proposals:"
                , propCard malawi
                , propCard dao
                , propCard dreps
                , propCard model.project
                ]
            , el [ El.width <| El.fillPortion 1 ] <| El.text ""
            ]


propCard : Project -> Element Msg
propCard proj =
    El.column
        [ El.width El.fill
        , El.padding 20
        , El.spacing 20
        , Bg.color <| El.rgb 0.1 0.1 0.1
        , Border.width 1
        , Border.dashed
        , Border.color <| El.rgb 0.5 0.5 0.5
        ]
        [ El.link
            [ El.spacing 15
            , Font.color <| El.rgb 0.65 0.85 1.0
            , Font.size 25
            , Font.bold
            ]
            { url = proj.link
            , label = El.paragraph [] [ El.text proj.title ]
            }
        , el
            [ El.alignRight
            , Font.color <| El.rgb 0.85 1.0 0.65
            ]
          <|
            El.text proj.ada
        , El.column [ El.spacing 10 ]
            [ el [ Font.bold, Font.size 25 ] <| El.text "Problem:"
            , contentBlock
                [ El.text proj.problem ]
            ]
        , El.column [ El.spacing 10 ]
            [ el [ Font.bold, Font.size 25 ] <| El.text "Solution:"
            , contentBlock
                [ El.text proj.solution ]
            ]
        ]


contentBlock : List (Element Msg) -> Element Msg
contentBlock =
    El.paragraph [ Font.justify, El.spacing 10 ]


dims : Float -> Element Msg
dims fs =
    El.image
        [ El.height <| El.px <| round <| fs * 0.8 -- - (fs * 1 / 6)
        , El.moveUp <| fs * 0.007
        ]
        { src = "assets/dims.svg"
        , description = "dims"
        }


projectDecoder : Decoder Project
projectDecoder =
    Jd.map5 Project
        (Jd.field "title" Jd.string)
        (Jd.field "link" Jd.string)
        (Jd.field "ada" Jd.string)
        (Jd.field "problem" Jd.string)
        (Jd.field "solution" Jd.string)


edges =
    { top = 0
    , bottom = 0
    , right = 0
    , left = 0
    }



-- Projects --


malawi : Project
malawi =
    { title = "Research: Strategically Competing with Mobile Money Markets in Malawi"
    , link = "https://cardano.ideascale.com/c/idea/106578"
    , ada = "₳15,000"
    , problem = "Mobile money is commonly used in southeast Africa. Cardano hasn't yet disrupted this market. There are untapped opportunities to improve people's ability to move money as well as Cardano's reach!"
    , solution = "    I see these transactions all the time. My solution is a local survey & detailed report about how Cardano wallet providers could work to make the switch to a Cardano-based solution as easy as possible."
    }


dao : Project
dao =
    { title = "Research: Real DAOs and Optimizing Governance for Parallel Experimentation"
    , link = "https://cardano.ideascale.com/c/idea/105979"
    , ada = "₳20,000"
    , problem = "Current DAOs don't really focus much on Autonomy. In a sense, DAOs aren't that Decentralized because they pool funds together and require majority votes to allocate them. Is mob rule even Organized???"
    , solution = "Are current DAOs are really DAOs? The goal is to produce detailed research and documentation of what it might mean to autonomously organize in a truly decentralized way! Can we maximize experiments???"
    }


dreps : Project
dreps =
    { title = "Research: Unique Pseudonymous Identification of DReps through Joint Content Creation"
    , link = "https://cardano.ideascale.com/c/idea/105563"
    , ada = "₳20,000"
    , problem = "Allowing DReps to remain anonymous has value, but there are some dangers to fair governance if very large whales decide to game the system. DReps should be encouraged to consider diverse perspectives."
    , solution = "                  Research detailing how community discussions can not only help DReps broaden their perspectives, but also be used to validate DReps as unique individuals, making things like quadratic voting possible."
    }
