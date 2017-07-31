module World.Components exposing (..)

import Vector2 exposing (Float2)
import Slime exposing (EntityID)
import Assets.Reference exposing (Sfx(..), Element(..), Sprite(..))


type alias Damagable x =
    { x
        | moveSpeed : Float
        , maxMoveSpeed : Float
        , health : Float
        , maxHealth : Float
    }


type alias PlayerStatus =
    { currentTime : Float
    , lastCast : Float
    , spell : Spell
    }


type alias Player =
    Damagable PlayerStatus


initPlayer : Player
initPlayer =
    { moveSpeed = 4, maxMoveSpeed = 4, currentTime = 0, lastCast = 0, health = 40, maxHealth = 40, spell = fizzle }


fizzle : Spell
fizzle =
    { effect =
        Proj
            { hit = Slow 0.1
            , projSpeed = 2
            , projLife = 2
            , penetrate = True
            , projectileArt = ProjSprite Fizzle
            }
    , castSpeed = 0.125
    , castsLeft = 0
    , maxCasts = 0
    , icon = SpellIcon Fizzle
    , sound = CastFizzle
    }


type alias EnemyStatus =
    { attackDamage : Float
    , sprite : Sprite
    , attackProgress : Maybe Float
    , attackSpeed : Float
    , attackSfx : Sfx
    }


type alias Enemy =
    Damagable EnemyStatus


hurt : Float -> Damagable x -> Damagable x
hurt amount ({ health } as me) =
    { me | health = min (health - amount) me.maxHealth }


type SpellType
    = Hurt Float
    | Slow Float
    | Explode SpellType Float2 Sprite
    | Composite SpellType SpellType
    | Random (List SpellType)


type alias ProjSpell =
    { hit : SpellType
    , projSpeed : Float
    , penetrate : Bool
    , projectileArt : Sprite
    , projLife : Float
    }


type alias AreaSpell =
    { enter : SpellType
    , areaArt : Sprite
    , entered : List EntityID
    }


type alias SelfSpell =
    { hit : SpellType }


type SpellClass
    = Proj ProjSpell
    | Mine ProjSpell
    | Self SelfSpell


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


type alias Platform =
    { sprite : Sprite
    , progress : Float
    , spells : List ( Float, Spell )
    }


type alias Particle =
    Projectile { size : Float2, maxLife : Float }


type alias OwnedProjectile x =
    Projectile { x | owner : EntityID }


type alias PlayerProjectile =
    OwnedProjectile
        { effect : SpellClass
        }
