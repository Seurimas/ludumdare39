module World.Power exposing (..)

import Random.Pcg as Pcg


type alias MagicalWorld x =
    { x | magicSeed : Pcg.Seed }


initMagicSeed =
    Pcg.initialSeed 666
