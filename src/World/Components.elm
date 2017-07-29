module World.Components exposing (..)

import Vector2 exposing (Float2)
import Slime exposing (EntityID)


type alias PlayerStatus =
    { moveSpeed : Float
    , currentTime : Float
    , lastCast : Float
    , castSpeed : Float
    }


type Player
    = Player PlayerStatus


initPlayer : Player
initPlayer =
    Player { moveSpeed = 4, currentTime = 0, lastCast = 0, castSpeed = 0.25 }


getPlayerSpeed : Player -> Float
getPlayerSpeed (Player { moveSpeed }) =
    moveSpeed


type Enemy
    = Goblin


type alias Projectile x =
    { x
        | pos : Float2
        , vel : Float2
    }


type alias OwnedProjectile =
    Projectile { owner : EntityID }
