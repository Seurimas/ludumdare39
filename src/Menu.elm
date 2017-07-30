module Menu exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import World exposing (..)
import Math exposing (Rectangle)


type Msg
    = StartGame
    | RestartGame
    | Options


update : Msg -> World -> ( World, Cmd msg )
update msg model =
    case msg of
        StartGame ->
            initializeWorld Playing model.assets ! []

        RestartGame ->
            initializeWorld Playing model.assets ! []

        _ ->
            model ! []


font =
    [ ( "font-family", "Papyrus, Copperplate, Optima,  Verdana" )
    , ( "font-size", "2em" )
    , ( "color", "#55ccff" )
    ]


dimensions ( width, height ) padding =
    [ ( "width", toString (width - padding * 2) ++ "px" )
    , ( "height", toString (height - padding * 2) ++ "px" )
    , ( "padding", toString padding ++ "px" )
    ]


sprite { x, y, width, height } url =
    div
        [ style
            ([ ( "background-position", "-" ++ toString x ++ "px -" ++ toString y ++ "px" )
             , ( "background-image", "url(" ++ url ++ ")" )
             ]
                ++ dimensions ( width, height ) 0
            )
        ]
        []


splash titleText =
    div
        [ style
            ([ ( "text-align", "center" ) ])
        ]
        [ span [ style [ ( "margin", "32px" ) ] ] [ text titleText ] ]


buttons =
    let
        buttonStyle extra =
            style
                ([ ( "margin", "32px" )
                 , ( "font-size", "1em" )
                 ]
                    ++ extra
                )

        startGame =
            button [ onClick StartGame, buttonStyle [ ( "color", "#55ccff" ) ] ] [ text "Fight the Horde!" ]

        options =
            button [ onClick Options, buttonStyle [] ] [ text "Options" ]
    in
        div
            [ style
                [ ( "margin", "64px" )
                , ( "display", "flex" )
                , ( "flex-direction", "column" )
                , ( "min-height", "200px" )
                ]
            ]
            [ startGame, options ]


renderMenu world =
    let
        screenSize =
            ( 500, 500 )
    in
        div [ style ((dimensions screenSize 32) ++ [ ( "background-color", "black" ) ] ++ font) ]
            [ splash "Mystic Power"
            , buttons
            ]


renderGameOver world =
    div [] []
