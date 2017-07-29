module World exposing (..)

import Slime exposing (..)
import Input exposing (InputState)
import Math exposing (Rectangle)
import Vector2 exposing (Float2)


type alias PlayerStatus =
    { moveSpeed : Float
    }


type Player
    = Player PlayerStatus


initPlayer : Player
initPlayer =
    Player { moveSpeed = 1 }


getPlayerSpeed : Player -> Float
getPlayerSpeed (Player { moveSpeed }) =
    moveSpeed


spawnPlayer : Float2 -> World -> World
spawnPlayer { x, y } world =
    let
        ( _, updatedWorld ) =
            forNewEntity world
                &=> ( transforms, { x = x, y = y, width = 1, height = 1 } )
                &=> ( player, initPlayer )
    in
        updatedWorld


type alias World =
    EntitySet
        { transforms : ComponentSet Rectangle
        , rotations : ComponentSet Float
        , player : ComponentSet Player
        , inputState : InputState
        }


world =
    { transforms = initComponents
    , rotations = initComponents
    , player = initComponents
    , inputState = initInputState
    , idSource = initIdSource
    }


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
