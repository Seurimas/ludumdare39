module Menu exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import World exposing (..)
import Math exposing (Rectangle)
import World.Platforms exposing (getSpellPicks)
import World.Spells
import Assets.Reference exposing (bottomLeft, size)


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
                        |> World.Platforms.spawnPlatform ( ( 0, 0 ), 0 )
            in
                newWorld ! []

        RestartGame ->
            let
                newWorld =
                    initializeWorld Playing model.assets model.seed model.magicSeed model.mainThemeNode
                        |> World.Platforms.spawnPlatform ( ( 0, 0 ), 0 )
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


spriteElem sprite =
    let
        ( blx, bly ) =
            bottomLeft sprite

        ( width, height ) =
            Assets.Reference.size sprite

        x =
            blx

        y =
            (512 - bly) - height
    in
        div
            [ style
                ([ ( "background-position", "-" ++ toString x ++ "px -" ++ toString y ++ "px" )
                 , ( "background-image", "url(resources/sprites.png)" )
                 , ( "overflow", "hidden" )
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


spellPick progress pickSpell index ( needed, spell ) =
    let
        offset =
            case index of
                0 ->
                    200

                1 ->
                    250

                2 ->
                    300

                _ ->
                    500

        icon =
            [ spriteElem spell.icon
            , text (toString spell.castsLeft)
            ]
    in
        div
            [ style
                [ ( "position", "absolute" )
                , ( "top", (toString offset) ++ "px" )
                , ( "right", "0px" )
                , ( "width", "50px" )
                , ( "height", "50px" )
                , ( "display", "flex" )
                , ( "flex-direction", "column" )
                ]
            , onClick (pickSpell spell)
            ]
            icon


spellPicks world pickSpell =
    let
        ( progress, spellList ) =
            getSpellPicks world
    in
        List.indexedMap (spellPick progress pickSpell) spellList


wrapGameWorld rendered world pickSpell =
    div
        [ style
            ([ ( "cursor", "none" ), ( "position", "relative" ) ]
                ++ dimensions ( 500, 500 ) 0
            )
        ]
        ([ rendered
         ]
            ++ spellPicks world pickSpell
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
