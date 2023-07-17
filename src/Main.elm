module Main exposing (..)

import Browser
import Element as El exposing (Element, el)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)


type Model
    = Empty


type Msg
    = NoOp


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
    ( Empty, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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
                [ El.paragraph
                    [ Font.justify
                    , El.spacing 10
                    , Font.color <| El.rgb 1.0 0.85 0.65
                    ]
                    [ el [ Font.bold ] <| El.text "Note: "
                    , El.text "This will be the landing page for all the progress I've made in my projects! Please come back and see my progress as time goes on!"
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
                , propCard 106578
                    "Research: Strategically Competing with Mobile Money Markets in Malawi"
                    "₳15,000"
                    "Mobile money is commonly used in southeast Africa. Cardano hasn't yet disrupted this market. There are untapped opportunities to improve people's ability to move money as well as Cardano's reach!"
                    "    I see these transactions all the time. My solution is a local survey & detailed report about how Cardano wallet providers could work to make the switch to a Cardano-based solution as easy as possible."
                , propCard 105979
                    "Research: Real DAOs and Optimizing Governance for Parallel Experimentation"
                    "₳20,000"
                    "Current DAOs don't really focus much on Autonomy. In a sense, DAOs aren't that Decentralized because they pool funds together and require majority votes to allocate them. Is mob rule even Organized???"
                    "Are current DAOs are really DAOs? The goal is to produce detailed research and documentation of what it might mean to autonomously organize in a truly decentralized way! Can we maximize experiments???"
                , propCard 105563
                    "Research: Unique Pseudonymous Identification of DReps through Joint Content Creation"
                    "₳20,000"
                    "Allowing DReps to remain anonymous has value, but there are some dangers to fair governance if very large whales decide to game the system. DReps should be encouraged to consider diverse perspectives."
                    "Research detailing how community discussions can not only help DReps broaden their perspectives, but also be used to validate DReps as unique individuals, making things like quadratic voting possible."
                , propCard 107701
                    "Dims: Distributed Idea Management System"
                    "₳45,000"
                    "Though collaboration is encouraged, the current ideation process in Catalyst incentivizes siloed competition. How can we make collaboration and convergent solutions the naturally beneficial choice?"
                    "Dims: The research and development of a tool for building and merging structured graphs of knowledge and code. Highlighting how efforts overlap between different projects and individual interests."
                ]
            , el [ El.width <| El.fillPortion 1 ] <| El.text ""
            ]


propCard : Int -> String -> String -> String -> String -> Element msg
propCard propNum label money problem solution =
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
            { url =
                "https://cardano.ideascale.com/c/idea/"
                    ++ String.fromInt propNum
            , label = El.paragraph [] [ El.text label ]
            }
        , el
            [ El.alignRight
            , Font.color <| El.rgb 0.85 1.0 0.65
            ]
          <|
            El.text money
        , El.column [ El.spacing 10 ]
            [ el [ Font.bold, Font.size 25 ] <| El.text "Problem:"
            , El.paragraph [ Font.justify, El.spacing 10 ]
                [ El.text problem ]
            ]
        , El.column [ El.spacing 10 ]
            [ el [ Font.bold, Font.size 25 ] <| El.text "Solution:"
            , El.paragraph [ Font.justify, El.spacing 10 ]
                [ El.text solution ]
            ]
        ]


edges =
    { top = 0
    , bottom = 0
    , right = 0
    , left = 0
    }
