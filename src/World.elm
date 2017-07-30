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


type GameState
    = MainMenu
    | Playing
    | GameOver


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
                        , assets : Maybe Assets
                        , gameState : GameState
                        }
                    )
                )
            )
        )


initializeWorld gameState assets =
    { transforms = initComponents
    , rotations = initComponents
    , player = initComponents
    , enemies = initComponents
    , playerProjectiles = initComponents
    , inputState = Input.initInputState
    , camera = initCamera
    , screenSize = initScreen
    , idSource = initIdSource
    , currentTime = 0
    , lastSpawn = 0
    , interval = 1
    , seed = World.Spawning.initSeed
    , magicSeed = initMagicSeed
    , assets = Debug.log "assets" assets
    , gameState = Debug.log "Gamestate" gameState
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


deletor =
    deleteEntity transforms
        &-> rotations
        &-> player
        &-> enemies
        &-> playerProjectiles
