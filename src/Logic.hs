{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Logic where

import Control.Monad.State
import Data.Maybe
import Miso
import System.Random

import GameModel
import InputModel

groupedByTwo :: Eq a => [a] -> [[a]]
groupedByTwo [x] = [[x]]
groupedByTwo [x, y] =
  if x == y
    then [[x, y]]
    else [[x], [y]]
groupedByTwo (x:y:xs) =
  if x == y
    then [x, y] : groupedByTwo xs
    else [x] : groupedByTwo (y : xs)
groupedByTwo [] = []

mergeTiles :: [Tile] -> Tile
mergeTiles [x] = x
mergeTiles [x, y] = Tile s (-1, -1) [x, y]
  where
    s = tileValue x + tileValue y

slideRow :: [Tile] -> ([Tile], Int)
slideRow r =
  ( take gridSize (map mergeTiles grouped ++ replicate gridSize Empty)
  , sum . map tileToInt . concat . filter (\x -> length x > 1) $ grouped)
  where
    grouped = groupedByTwo . filter (/= Empty) $ r

rotatedGrid :: Direction -> Grid -> Grid
rotatedGrid D = rotateGrid
rotatedGrid R = rotateGrid . rotateGrid
rotatedGrid U = rotateGrid . rotateGrid . rotateGrid
rotatedGrid _ = id

slidGrid :: Direction -> Grid -> Grid
slidGrid U = rotateGrid
slidGrid R = rotateGrid . rotateGrid
slidGrid D = rotateGrid . rotateGrid . rotateGrid
slidGrid _ = id

slideGrid :: Direction -> Grid -> (Grid, Int)
slideGrid None grid = (grid, 0)
slideGrid dir grid = (newGrid, scoreGained)
  where
    posGrid = updatePosition grid
    rowsWithScores = map slideRow . (\(Grid h) -> h) . rotatedGrid dir $ posGrid
    slidRotatedGrid = Grid (map fst rowsWithScores)
    scoreGained = sum . map snd $ rowsWithScores
    newGrid = slidGrid dir slidRotatedGrid

slideGameState :: GameState -> GameState
slideGameState state@GameState {..} =
  if newGrid == grid
    then state
    else state
         { grid = newGrid
         , score = newScore
         , bestScore = newBest
         , scoreAdd = gotScore
         }
  where
    (newGrid, gotScore) = slideGrid direction grid
    newScore = score + gotScore
    newBest = max bestScore newScore

gameLost :: Grid -> Bool
gameLost g = (g /= emptyGrid) && all (== g) [up, down, left, right]
  where
    up = fst . slideGrid U $ g
    down = fst . slideGrid D $ g
    left = fst . slideGrid L $ g
    right = fst . slideGrid R $ g

gameWon :: Grid -> Bool
gameWon (Grid g) = Number 2048 `elem` concat g

lose :: GameState -> GameState
lose gameState = gameState {gameProgress = GameOver}

win :: GameState -> GameState
win gameState = gameState {gameProgress = Won}

tile2Probability :: Float
tile2Probability = 0.9

newTile :: Float -> Tile
newTile x
  | x < tile2Probability = Number 2
  | otherwise = Number 4

emptyTiles :: Grid -> [(Int, Int)]
emptyTiles =
  map (\(_, i, j) -> (i, j)) .
  filter (\(t, _, _) -> t == Empty) . tilesWithCoordinates

newTileIndex :: Float -> Grid -> Maybe (Int, Int)
newTileIndex x g =
  case emptyTileIndices of
    [] -> Nothing
    _ -> Just (emptyTileIndices !! idx)
  where
    emptyTileIndices = emptyTiles g
    idx = (floor . (* x) . fromIntegral . length $ emptyTileIndices) :: Int

placeRandomTile :: GameState -> GameState
placeRandomTile gameState@GameState {..} =
  if isNothing tileIndex
    then gameState
    else gameState
         { grid = setTile (fromMaybe (0, 0) tileIndex) grid $ newTile float2
         , randomSeed = nSeed
         }
  where
    (float1, stdGen1) = random . mkStdGen $ randomSeed
    (float2, stdGen2) = random stdGen1
    (nSeed, _) = random stdGen2
    tileIndex = newTileIndex float1 grid

newGame :: GameState -> GameState
newGame state@GameState {..} = newGame
  where
    newGame =
      placeRandomTile . placeRandomTile $
      defaultGame {randomSeed = randomSeed, bestScore = bestScore}

stepSlide :: GameState -> GameState
stepSlide state =
  if grid pushedState == grid state
    then state
    else placeRandomTile pushedState
  where
    pushedState = slideGameState state {drawScoreAdd = 0}

step :: GameState -> GameState
step state@GameState {..} =
  if | gameProgress == Won || gameProgress == GameOver -> state
     | gameWon grid -> win state
     | gameLost grid -> lose state
     | direction /= None -> stepSlide state
     | otherwise -> state

updateGameState :: Action -> Transition GameState Action
updateGameState = \case
  Init ->
    issue NewGame
  Sync ->
    modify $ \m -> m { drawScoreAdd = scoreAdd m }
  NewGame -> do
    modify newGame
    issue Sync
  Continue ->
    modify $ \m -> m { gameProgress = Continuing }
  GetArrows arr -> do
    modify $ \m -> m { direction = toDirection arr }
    modify step
    issue Sync
  TouchStart pointer ->
    modify $ \m -> m { prevTouch = Just pointer }
  TouchEnd pointer -> do
    modify $ \m -> m { prevTouch = Nothing }
    mapM_ action =<< prevTouch <$> get
      where
        action touch
          = issue
          $ swipe (client touch) (client pointer)
