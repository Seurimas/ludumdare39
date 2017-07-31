module LudumDare39 exposing (main)

{-|
@docs main
-}

import Slime.Engine exposing (..)
import Assets.Loading exposing (Assets, load)
import Assets.Reference exposing (playMusic, stopMusic, Sfx(GameOverSfx), playSfx)
import World exposing (..)
import World.Casting
import World.Spells
import World.Movement
import World.Enemies
import World.Platforms
import World.Spawning
import World.Projectiles
import World.Components exposing (Spell)
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
import Whistle.Types exposing (RawNode)
import Random.Pcg exposing (Seed)
import Random exposing (generate)
import Random.Pcg.Interop exposing (fission)


type LDMsg
    = InputMsg Input.Msg
    | Load Assets
    | Menu Menu.Msg
    | StartMusic
    | StopMusic
    | MusicStarted RawNode
    | MagicSeed Seed
    | SpawnSeed Seed
    | PickSpell Spell
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


playMainTheme world =
    world ! [ playMusic (\rawNode -> (MusicStarted rawNode)) world.assets ]


stopMainTheme world =
    { world | mainThemeNode = Nothing } ! [ stopMusic squelchSfxMsg world.mainThemeNode ]


playGameOver ( world, cmds ) =
    world ! [ cmds, playSfx squelchSfxMsg GameOverSfx 1 world.assets ]


squelchSfxMsg =
    (\rawNode -> Noop)


engine : Engine World LDMsg
engine =
    let
        systems =
            [ timedSystem World.Movement.movePlayer
            , untimedSystem World.Movement.facePlayer
            , timedSystem World.Movement.cameraFollow
            , timedSystem World.Platforms.progressPlatforms
            , timedSystem (World.Spawning.spawningSystem (World.Enemies.spawns ++ World.Platforms.spawns))
            , systemWith { timing = timed, options = cmds } (World.Enemies.chasePlayer squelchSfxMsg)
            , timedSystem World.Enemies.separateEnemies
            , systemWith { timing = timed, options = cmds } (World.Casting.playerCasting squelchSfxMsg)
            , untimedSystem World.Spells.switchSpell
            , systemWith { timing = untimed, options = deletes } (World.Enemies.cullDead)
            , systemWith { timing = timed, options = deletes } (World.Projectiles.projectileStep playerProjectiles)
            , systemWith { timing = timed, options = deletes } (World.Projectiles.projectileStep particles)
            , systemWith { timing = timed, options = deletes } World.Projectiles.playerProjectileStep
            , systemWith { timing = untimed, options = cmds } (World.endGame stopMainTheme playGameOver)
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
            , Slime.Engine.listener
                (\msg ->
                    case msg of
                        PickSpell spell ->
                            World.Platforms.pickSpell spell

                        _ ->
                            identity
                )
            ]
    in
        Slime.Engine.initEngine deletor systems listeners


update msg model =
    let
        updateEngine _ =
            case msg of
                Msg ldMsg ->
                    case ldMsg of
                        MusicStarted rawNode ->
                            { model | mainThemeNode = Just rawNode } ! []

                        StartMusic ->
                            playMainTheme model |> mapCmds

                        StopMusic ->
                            stopMainTheme model |> mapCmds

                        _ ->
                            engineUpdate engine msg model

                _ ->
                    engineUpdate engine msg model

        updateMenu menuMsg =
            Menu.update menuMsg model

        mapCmds =
            (\( model, cmds ) -> ( model, cmds |> Cmd.map Msg ))
    in
        case model.gameState of
            Playing ->
                updateEngine ()

            _ ->
                case msg of
                    Msg ldMsg ->
                        case ldMsg of
                            Load assets ->
                                playMainTheme { model | assets = Just assets } |> mapCmds

                            MusicStarted rawNode ->
                                { model | mainThemeNode = Just rawNode } ! []

                            StartMusic ->
                                playMainTheme model |> mapCmds

                            StopMusic ->
                                stopMainTheme model |> mapCmds

                            MagicSeed seed ->
                                { model | magicSeed = seed } ! []

                            SpawnSeed seed ->
                                { model | seed = seed } ! []

                            Menu menuMsg ->
                                updateMenu menuMsg

                            _ ->
                                model ! []

                    _ ->
                        model ! []


renderGame world =
    wrapGameWorld
        (Game.render
            { time = world.currentTime
            , camera = world.camera
            , size = Vector2.map floor world.screenSize
            }
            (renderWorld world)
        )
        world
        PickSpell


render world =
    case world.gameState of
        Playing ->
            renderGame world |> Html.map Msg

        _ ->
            renderMenu world |> Html.map (\msg -> Msg (Menu msg))


{-| -}
main : Program Never World (Slime.Engine.Message LDMsg)
main =
    Html.program
        { init =
            (initializeWorld MainMenu Nothing (Random.Pcg.initialSeed 100) (Random.Pcg.initialSeed 100) Nothing)
                ! [ Task.attempt acceptAssets load |> Cmd.map Msg
                  , generate (Msg << MagicSeed) fission
                  , generate (Msg << SpawnSeed) fission
                  ]
        , subscriptions = subs
        , update = update
        , view = render
        }
