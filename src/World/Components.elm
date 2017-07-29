module World.Components exposing (..)

import Vector2 exposing (Float2)
import Slime exposing (EntityID)


type alias PlayerStatus =
    { moveSpeed : Float
    , currentTime : Float
    , lastCast : Float
    , castSpeed : Float
    }


type alias Player =
    PlayerStatus


initPlayer : Player
initPlayer =
    { moveSpeed = 4, currentTime = 0, lastCast = 0, castSpeed = 0.25 }


getPlayerSpeed : Player -> Float
getPlayerSpeed { moveSpeed } =
    moveSpeed


type Enemy
    = Goblin


type alias Projectile x =
    { x
        | pos : Float2
        , vel : Float2
        , lifeLeft : Float
    }


type alias OwnedProjectile =
    Projectile { owner : EntityID }
