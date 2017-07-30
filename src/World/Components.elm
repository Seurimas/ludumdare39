module World.Components exposing (..)

import Vector2 exposing (Float2)
import Slime exposing (EntityID)
import Assets.Reference exposing (Sfx(..), Sprite(ProjFizzle, SpellFizzle))


type alias Damagable x =
    { x
        | health : Float
        , maxHealth : Float
    }


type alias PlayerStatus =
    { moveSpeed : Float
    , currentTime : Float
    , lastCast : Float
    , spell : Spell
    }


type alias Player =
    Damagable PlayerStatus


initPlayer : Player
initPlayer =
    { moveSpeed = 4, currentTime = 0, lastCast = 0, health = 100, maxHealth = 100, spell = fizzle }


fizzle : Spell
fizzle =
    { effect =
        Proj
            { hit = Hurt 0
            , projSpeed = 2
            , penetrate = True
            , projectileArt = ProjFizzle
            }
    , castSpeed = 0.125
    , castsLeft = 0
    , maxCasts = 0
    , icon = SpellFizzle
    , sound = Fizzle
    }


type alias EnemyStatus =
    { moveSpeed : Float
    , attackDamage : Float
    , sprite : Sprite
    , attackProgress : Maybe Float
    , attackSpeed : Float
    }


type alias Enemy =
    Damagable EnemyStatus


hurt : Float -> Damagable x -> Damagable x
hurt amount ({ health } as me) =
    { me | health = health - amount }


type SpellType
    = Hurt Float
    | Slow Float
    | Composite SpellType SpellType


type alias ProjSpell =
    { hit : SpellType
    , projSpeed : Float
    , penetrate : Bool
    , projectileArt : Sprite
    }


type alias AreaSpell =
    { enter : SpellType
    , areaArt : Sprite
    , entered : List EntityID
    }


type SpellClass
    = Proj ProjSpell
    | Area AreaSpell


type alias Spell =
    { effect : SpellClass
    , castSpeed : Float
    , castsLeft : Int
    , maxCasts : Int
    , icon : Sprite
    , sound : Sfx
    }


type alias Projectile x =
    { x
        | pos : Float2
        , vel : Float2
        , lifeLeft : Float
        , sprite : Sprite
    }


type alias OwnedProjectile x =
    Projectile { x | owner : EntityID }


type alias PlayerProjectile =
    OwnedProjectile
        { effect : SpellClass
        }
