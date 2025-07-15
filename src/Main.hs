-- | Haskell language pragma
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import System.Random

import GameModel
import InputModel
import Logic
import Rendering

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

-- | Entry point for a miso application
main :: IO ()
main = do
  stdGen <- getStdGen
  let (seed, _) = random stdGen
      model = defaultGame { randomSeed = seed }
  run $ startComponent (component model updateGameState display)
    { initialAction = Just Init
    , subs = [arrowsSub GetArrows]
    , events = pointerEvents <> defaultEvents
    , logLevel = Off
    }
