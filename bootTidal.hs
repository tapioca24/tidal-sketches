-- Custom boot file for TidalCycles for VSCode
-- ref. https://github.com/tidalcycles/vscode-tidalcycles/blob/master/src/tidal.ts

:set -XOverloadedStrings
:set prompt ""
:set prompt-cont ""

import Sound.Tidal.Context

-- total latency = oLatency + cFrameTimespan
tidal <- startTidal (superdirtTarget {oLatency = 0.1, oAddress = "127.0.0.1", oPort = 57120}) (defaultConfig {cFrameTimespan = 1/20})

:{
let p = streamReplace tidal
    hush = streamHush tidal
    list = streamList tidal
    mute = streamMute tidal
    unmute = streamUnmute tidal
    solo = streamSolo tidal
    unsolo = streamUnsolo tidal
    once = streamOnce tidal
    asap = once
    nudgeAll = streamNudgeAll tidal
    all = streamAll tidal
    resetCycles = streamResetCycles tidal
    setcps = asap . cps
    xfade i = transition tidal True (Sound.Tidal.Transition.xfadeIn 4) i
    xfadeIn i t = transition tidal True (Sound.Tidal.Transition.xfadeIn t) i
    histpan i t = transition tidal True (Sound.Tidal.Transition.histpan t) i
    wait i t = transition tidal True (Sound.Tidal.Transition.wait t) i
    waitT i f t = transition tidal True (Sound.Tidal.Transition.waitT f t) i
    jump i = transition tidal True (Sound.Tidal.Transition.jump) i
    jumpIn i t = transition tidal True (Sound.Tidal.Transition.jumpIn t) i
    jumpIn' i t = transition tidal True (Sound.Tidal.Transition.jumpIn' t) i
    jumpMod i t = transition tidal True (Sound.Tidal.Transition.jumpMod t) i
    mortal i lifespan release = transition tidal True (Sound.Tidal.Transition.mortal lifespan release) i
    interpolate i = transition tidal True (Sound.Tidal.Transition.interpolate) i
    interpolateIn i t = transition tidal True (Sound.Tidal.Transition.interpolateIn t) i
    clutch i = transition tidal True (Sound.Tidal.Transition.clutch) i
    clutchIn i t = transition tidal True (Sound.Tidal.Transition.clutchIn t) i
    anticipate i = transition tidal True (Sound.Tidal.Transition.anticipate) i
    anticipateIn i t = transition tidal True (Sound.Tidal.Transition.anticipateIn t) i
    forId i t = transition tidal False (Sound.Tidal.Transition.mortalOverlay t) i
    d1 = p 1 . (|< orbit 0)
    d2 = p 2 . (|< orbit 1)
    d3 = p 3 . (|< orbit 2)
    d4 = p 4 . (|< orbit 3)
    d5 = p 5 . (|< orbit 4)
    d6 = p 6 . (|< orbit 5)
    d7 = p 7 . (|< orbit 6)
    d8 = p 8 . (|< orbit 7)
    d9 = p 9 . (|< orbit 8)
    d10 = p 10 . (|< orbit 9)
    d11 = p 11 . (|< orbit 10)
    d12 = p 12 . (|< orbit 11)
    d13 = p 13
    d14 = p 14
    d15 = p 15
    d16 = p 16
:}

:{
let setI = streamSetI tidal
    setF = streamSetF tidal
    setS = streamSetS tidal
    setR = streamSetI tidal
    setB = streamSetB tidal
:}

-- Custom functions

import qualified Sound.Tidal.Tempo as T
import Data.Fixed (mod')

:{
let
    -- ref. https://qiita.com/s2hap/items/88ab4ebb45882f7b339d
    resetCyclesTo n = T.changeTempo (sTempoMV tidal) (\t tempo -> tempo {T.atTime = t, T.atCycle = n})

    -- ref. https://gist.github.com/kindohm/f91dbdcff4ed9fbd83ed5524f1eef8bd
    gtfo = (const $ s "~")
    rip a b p = within (0.25, 0.75) (slow 2 . rev . stut 8 a b) p
    spike p = ((#delay 0.3) . (#delaytime (range 0.03 0.003 $ saw)) . (#delayfeedback 0.98)) $ p
    spike' d p = ((#delay d) . (#delaytime (range 0.03 0.003 $ saw)) . (#delayfeedback 0.98)) $ p

    -- ref. https://twitter.com/satoruki/status/1437827081736359936
    whenmod' a b f pat = innerJoin $ (\a' b' -> _whenmod' a' b' f pat) <$> a <*> b
    _whenmod' a b = whenT (\t -> ((t `mod'` a) < b))
    betweenmod a b c f pat = innerJoin $ (\a' b' c' -> _betweenmod' a' b' c' f pat) <$> a <*> b <*> c
    _betweenmod' a b c = whenT ((\t -> ((t `mod'` a) < b) && (t `mod'` a) >= c))
:}

:set prompt "tidal> "
