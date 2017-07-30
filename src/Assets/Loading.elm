module Assets.Loading exposing (..)

import WebGL.Texture as Texture exposing (Texture)
import Whistle.Types exposing (Buffer)
import Whistle.Native
import Whistle
import Task


type alias Assets =
    { spriteMap : Texture
    , fireWav : Buffer
    , fizzleWav : Buffer
    , newSpellWav : Buffer
    }


loadWav path =
    (Whistle.Native.getAudioData ("./resources/" ++ path))


loadTexture path =
    Texture.load ("./resources/" ++ path)
        |> Task.mapError (\err -> "Texture error: " ++ path)


load =
    loadWav "fire.wav"
        |> Task.andThen
            (\fire ->
                loadWav "fizzle.wav"
                    |> Task.andThen
                        (\fizzle ->
                            loadWav "newSpell.wav"
                                |> Task.andThen
                                    (\newSpell ->
                                        loadTexture "sprites.png"
                                            |> Task.map
                                                (\spriteMap ->
                                                    { spriteMap = spriteMap
                                                    , fireWav = fire
                                                    , fizzleWav = fizzle
                                                    , newSpellWav = newSpell
                                                    }
                                                )
                                    )
                        )
            )
