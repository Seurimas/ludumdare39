module World.Components exposing (..)

import Vector2 exposing (Float2)
import Slime exposing (EntityID)


type alias Damagable x =
    { x
        | health : Float
        , maxHealth : Float
    }


type alias PlayerStatus =
    { moveSpeed : Float
    , currentTime : Float
    , lastCast : Float
    , castSpeed : Float
    }


type alias Player =
    Damagable PlayerStatus


initPlayer : Player
initPlayer =
    { moveSpeed = 4, currentTime = 0, lastCast = 0, castSpeed = 0.25, health = 100, maxHealth = 100 }


type alias EnemyStatus =
    { moveSpeed : Float
    , attackDamage : Float
    }


type alias Enemy =
    Damagable EnemyStatus


initGoblin : Enemy
initGoblin =
    { moveSpeed = 4
    , attackDamage = 1
    , health = 4
    , maxHealth = 4
    }


hurt : Float -> Damagable x -> Damagable x
hurt amount ({ health } as me) =
    { me | health = health - amount }


type alias Projectile x =
    { x
        | pos : Float2
        , vel : Float2
        , lifeLeft : Float
    }


type alias OwnedProjectile x =
    Projectile { x | owner : EntityID }


type alias PlayerProjectile =
    OwnedProjectile
        { damage : Float
        }
