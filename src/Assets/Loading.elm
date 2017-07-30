module Assets.Loading exposing (..)

import WebGL.Texture as Texture exposing (Texture)
import Whistle.Types exposing (Buffer)
import Whistle.Native
import Whistle
import Task
import Dict exposing (Dict, fromList)


type alias Assets =
    { spriteMap : Texture
    , buffers : Dict String Buffer
    }


sounds =
    [ "fire.wav"
    , "fizzle.wav"
    , "newSpell.wav"
    , "goblin.wav"
    , "hobgoblin.wav"
    , "hulk.wav"
    , "mainTheme.wav"
    , "gameOver.wav"
    ]


loadWav path =
    (Whistle.Native.getAudioData ("./resources/" ++ path)) |> Task.map (\buffer -> ( path, buffer ))


loadWavs paths =
    Task.sequence (paths |> List.map loadWav)


loadTexture path =
    Texture.load ("./resources/" ++ path)
        |> Task.mapError (\err -> "Texture error: " ++ path)


load : Task.Task String Assets
load =
    loadWavs sounds
        |> Task.map fromList
        |> Task.andThen
            (\buffers ->
                loadTexture "sprites.png"
                    |> Task.map
                        (\spriteMap ->
                            { spriteMap = spriteMap
                            , buffers = buffers
                            }
                        )
            )
