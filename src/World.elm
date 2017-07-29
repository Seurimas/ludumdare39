module World exposing (..)

import Slime exposing (..)
import World.Input as Input exposing (InputState)
import World.View as View exposing (initCamera, initScreen)
import World.Spawning
import World.Components exposing (..)
import Math exposing (Rectangle)
import Vector2 exposing (Float2)
import Random.Pcg


spawnPlayer : Float2 -> World -> World
spawnPlayer ( x, y ) world =
    let
        ( _, updatedWorld ) =
            forNewEntity world
                &=> ( transforms, { x = x, y = y, width = 1, height = 1 } )
                &=> ( player, initPlayer )
    in
        updatedWorld


type alias World =
    EntitySet
        (World.Spawning.SpawningWorld
            (View.ViewableWorld
                (Input.InteractiveWorld
                    { transforms : ComponentSet Rectangle
                    , rotations : ComponentSet Float
                    , player : ComponentSet Player
                    , enemies : ComponentSet Enemy
                    , playerProjectiles : ComponentSet OwnedProjectile
                    }
                )
            )
        )


world =
    { transforms = initComponents
    , rotations = initComponents
    , player = initComponents
    , enemies = initComponents
    , playerProjectiles = initComponents
    , inputState = Input.initInputState
    , camera = initCamera
    , screenSize = initScreen
    , idSource = initIdSource
    , seed = World.Spawning.initSeed
    , currentTime = 0
    , lastSpawn = -10
    , interval = 5
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
