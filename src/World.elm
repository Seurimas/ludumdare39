module World exposing (..)

import Slime exposing (..)
import World.Input as Input exposing (InputState)
import World.View as View exposing (initCamera, initScreen)
import World.Spawning
import World.Components exposing (..)
import World.Power exposing (..)
import Math exposing (Rectangle)
import Vector2 exposing (Float2)
import Random.Pcg
import Assets.Loading exposing (Assets)
import Whistle.Types exposing (RawNode)


type GameState
    = MainMenu
    | Playing
    | GameOver
    | Options


spawnPlayer : Float2 -> World -> World
spawnPlayer ( x, y ) world =
    let
        ( _, updatedWorld ) =
            forNewEntity world
                &=> ( transforms, { x = x, y = y, width = 1, height = 1 } )
                &=> ( player, initPlayer )
                &=> ( rotations, 0 )
    in
        updatedWorld


endGame : (World -> ( World, Cmd msg )) -> (( World, Cmd msg ) -> ( World, Cmd msg )) -> World -> ( World, Cmd msg )
endGame stopMusic playGameOver world =
    let
        players =
            world &. (entities player)

        deadPlayers =
            players |> List.filter (\{ a } -> a.health <= 0)
    in
        if List.length deadPlayers > 0 then
            playGameOver (stopMusic { world | gameState = GameOver })
        else
            world ! []


type alias World =
    EntitySet
        (MagicalWorld
            (World.Spawning.SpawningWorld
                (View.ViewableWorld
                    (Input.InteractiveWorld
                        { transforms : ComponentSet Rectangle
                        , rotations : ComponentSet Float
                        , player : ComponentSet Player
                        , enemies : ComponentSet Enemy
                        , playerProjectiles : ComponentSet PlayerProjectile
                        , particles : ComponentSet Particle
                        , platforms : ComponentSet Platform
                        , assets : Maybe Assets
                        , gameState : GameState
                        , mainThemeNode : Maybe RawNode
                        }
                    )
                )
            )
        )


initializeWorld gameState assets seed magicSeed themeNode =
    { transforms = initComponents
    , rotations = initComponents
    , player = initComponents
    , enemies = initComponents
    , playerProjectiles = initComponents
    , particles = initComponents
    , platforms = initComponents
    , inputState = Input.initInputState
    , camera = initCamera
    , screenSize = initScreen
    , idSource = initIdSource
    , currentTime = 0
    , lastSpawn = 0
    , interval = 1
    , seed = seed
    , magicSeed = magicSeed
    , assets = assets
    , gameState = gameState
    , mainThemeNode = themeNode
    }
        |> spawnPlayer ( 0, 0 )


transforms =
    { getter = .transforms
    , setter = \newTransforms world -> { world | transforms = newTransforms }
    }


rotations =
    { getter = .rotations
    , setter = \rotations world -> { world | rotations = rotations }
    }


player =
    { getter = .player
    , setter = \player world -> { world | player = player }
    }


enemies =
    { getter = .enemies
    , setter = \enemies world -> { world | enemies = enemies }
    }


playerProjectiles =
    { getter = .playerProjectiles
    , setter = \playerProjectiles world -> { world | playerProjectiles = playerProjectiles }
    }


particles =
    { getter = .particles
    , setter = \particles world -> { world | particles = particles }
    }


platforms =
    { getter = .platforms
    , setter = \platforms world -> { world | platforms = platforms }
    }


deletor =
    deleteEntity transforms
        &-> rotations
        &-> player
        &-> enemies
        &-> playerProjectiles
        &-> particles
        &-> platforms
