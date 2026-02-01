-- | Haskell language pragma
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import qualified Data.Map as M
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
  startComponent (M.singleton "click" BUBBLE) (component model updateGameState display)
    { mount = Just Init
    , subs = [arrowsSub GetArrows]
#ifndef WASM
    , styles = [ Href "./static/main.css" ]
#endif
    }
