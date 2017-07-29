module LudumDare39 exposing (..)

{-| -}

import Slime.Engine exposing (..)
import World exposing (..)
import World.Movement
import World.Enemies
import World.Spawning
import World.Render exposing (renderWorld)
import World.Input as Input exposing (..)
import Html exposing (Html, div)
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing (Camera)
import Color exposing (Color)
import Vector2


type Msg
    = InputMsg Input.Msg


subs m =
    Sub.map InputMsg Input.subs
        |> engineSubs


engine =
    let
        systems =
            [ timedSystem World.Movement.movePlayer
            , timedSystem World.Movement.cameraFollow
            , timedSystem (World.Spawning.spawningSystem (World.Enemies.spawns))
            , timedSystem World.Enemies.chasePlayer
            ]

        listeners =
            [ Slime.Engine.listener Input.listener
                |> listenerMap
                    (\msg ->
                        case msg of
                            InputMsg x ->
                                x
                    )
                    InputMsg
            ]
    in
        Slime.Engine.initEngine deletor systems listeners


updateWorld =
    Slime.Engine.applySystems engine


takeMessage =
    Slime.Engine.applyListeners engine


update msg model =
    engineUpdate engine msg model


render world =
    Game.render
        { time = 0
        , camera = world.camera
        , size = Vector2.map floor world.screenSize
        }
        (renderWorld world)


main =
    Html.program
        { init = world ! []
        , subscriptions = subs
        , update = update
        , view = render
        }
