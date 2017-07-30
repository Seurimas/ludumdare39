module LudumDare39 exposing (main)

{-|
@docs main
-}

import Slime.Engine exposing (..)
import Assets.Loading exposing (Assets, load)
import World exposing (..)
import World.Casting
import World.Spells
import World.Movement
import World.Enemies
import World.Spawning
import World.Projectiles
import World.Render exposing (renderWorld)
import Menu exposing (..)
import World.Input as Input
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing (Camera)
import Color exposing (Color)
import Vector2
import Task


type LDMsg
    = InputMsg Input.Msg
    | Load Assets
    | Menu Menu.Msg
    | Noop


acceptAssets : Result String Assets -> LDMsg
acceptAssets results =
    case results of
        Ok assets ->
            Load assets

        _ ->
            Debug.crash "Failed to load assets."


subs m =
    Sub.map InputMsg Input.subs
        |> engineSubs


engine : Engine World LDMsg
engine =
    let
        systems =
            [ timedSystem World.Movement.movePlayer
            , untimedSystem World.Movement.facePlayer
            , timedSystem World.Movement.cameraFollow
            , timedSystem (World.Spawning.spawningSystem (World.Enemies.spawns))
            , timedSystem World.Enemies.chasePlayer
            , timedSystem World.Enemies.separateEnemies
            , systemWith { timing = timed, options = cmds } (World.Casting.playerCasting (\rawNode -> Noop))
            , untimedSystem World.Spells.switchSpell
            , systemWith { timing = untimed, options = deletes } (World.Enemies.cullDead)
            , systemWith { timing = timed, options = deletes } (World.Projectiles.projectileStep playerProjectiles)
            , systemWith { timing = timed, options = deletes } World.Projectiles.playerProjectileStep
            ]

        listeners =
            [ Slime.Engine.listener Input.listener
                |> listenerMap
                    (\msg ->
                        case msg of
                            InputMsg x ->
                                x

                            _ ->
                                Input.Noop
                    )
                    InputMsg
            , listener
                (\msg world ->
                    case msg of
                        Load assets ->
                            { world | assets = Just assets }

                        _ ->
                            world
                )
            ]
    in
        Slime.Engine.initEngine deletor systems listeners


update msg model =
    case model.assets of
        Nothing ->
            case msg of
                Msg ldMsg ->
                    case ldMsg of
                        Load _ ->
                            engineUpdate engine msg model

                        Menu menuMsg ->
                            Menu.update menuMsg model

                        _ ->
                            model ! []

                -- Ignore other messages until loaded
                _ ->
                    model ! []

        _ ->
            case msg of
                Msg ldMsg ->
                    case ldMsg of
                        Menu menuMsg ->
                            Menu.update menuMsg model

                        _ ->
                            engineUpdate engine msg model

                -- Ignore other messages until loaded
                _ ->
                    engineUpdate engine msg model


renderGame world =
    div
        [ style [ ( "cursor", "none" ) ] ]
        [ Game.render
            { time = world.currentTime
            , camera = world.camera
            , size = Vector2.map floor world.screenSize
            }
            (renderWorld world)
        ]


render world =
    case world.gameState of
        MainMenu ->
            renderMenu world |> Html.map (\msg -> Msg (Menu msg))

        Playing ->
            renderGame world

        GameOver ->
            renderGameOver world


{-| -}
main : Program Never World (Slime.Engine.Message LDMsg)
main =
    Html.program
        { init = initializeWorld MainMenu Nothing ! [ Task.attempt acceptAssets load |> Cmd.map Msg ]
        , subscriptions = subs
        , update = update
        , view = render
        }
