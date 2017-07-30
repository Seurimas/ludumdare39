module Menu exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import World exposing (..)
import Math exposing (Rectangle)


type Msg
    = StartGame
    | RestartGame
    | ToMainMenu
    | ToOptions


update : Msg -> World -> ( World, Cmd msg )
update msg model =
    case msg of
        StartGame ->
            let
                newWorld =
                    initializeWorld Playing model.assets model.seed model.magicSeed model.mainThemeNode
            in
                newWorld ! []

        RestartGame ->
            let
                newWorld =
                    initializeWorld Playing model.assets model.seed model.magicSeed model.mainThemeNode
            in
                newWorld ! []

        ToMainMenu ->
            { model | gameState = MainMenu } ! []

        ToOptions ->
            { model | gameState = Options } ! []


font =
    [ ( "font-family", "Papyrus, Copperplate, Optima,  Verdana" )
    , ( "font-size", "2em" )
    , ( "color", "#55ccff" )
    ]


screenSize =
    ( 500, 500 )


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


buttonStyle extra =
    style
        ([ ( "margin", "32px" )
         , ( "font-size", "1em" )
         ]
            ++ extra
        )


menuStyle extra =
    style
        ([ ( "margin", "64px" )
         , ( "display", "flex" )
         , ( "flex-direction", "column" )
         , ( "min-height", "200px" )
         ]
            ++ extra
        )


renderMainMenu world =
    let
        buttons =
            let
                startGame =
                    button [ onClick StartGame, buttonStyle [ ( "color", "#55ccff" ) ] ] [ text "Fight the Horde!" ]

                options =
                    button [ onClick ToOptions, buttonStyle [] ] [ text "Options" ]
            in
                div
                    [ menuStyle [] ]
                    [ startGame, options ]
    in
        div [ style ((dimensions screenSize 32) ++ [ ( "background-color", "black" ) ] ++ font) ]
            [ splash "Mystic Power"
            , buttons
            ]


renderGameOver world =
    let
        buttons =
            let
                restart =
                    button [ onClick RestartGame, buttonStyle [ ( "color", "#55ccff" ) ] ] [ text "Reclaim your power!" ]

                mainMenu =
                    button [ onClick ToMainMenu, buttonStyle [] ] [ text "Main Menu" ]
            in
                div
                    [ menuStyle [] ]
                    [ restart, mainMenu ]
    in
        div [ style ((dimensions screenSize 32) ++ [ ( "background-color", "black" ) ] ++ font) ]
            [ splash "You have been taken by the hoard!"
            , buttons
            ]


renderOptions world =
    div [] []


renderMenu world =
    case world.gameState of
        MainMenu ->
            renderMainMenu world

        GameOver ->
            renderGameOver world

        Options ->
            renderOptions world

        _ ->
            renderMainMenu world
