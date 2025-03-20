-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Haskell module declaration
module Main where

import Data.Map
-- | Miso framework import
import Miso
import Miso.String (MisoString, ms)
import System.Random

import GameModel
import InputModel
import Logic
import Rendering

-- | Entry point for a miso application
main :: IO ()
main = do
  stdGen <- getStdGen
  let (seed, _) = random stdGen
      model = defaultGame { randomSeed = seed }
  startApp (defaultApp model updateGameState display)
    { initialAction = Just Init
    , subs = [arrowsSub GetArrows]
    , events = pointerEvents <> defaultEvents
    , logLevel = Off
    }
