{-# LANGUAGE RecordWildCards #-}

module InputModel where

import Miso

data Direction
  = U
  | D
  | L
  | R
  | None
  deriving (Show, Eq)

data Action
  = Init
  | NewGame
  | GetArrows Arrows
  | Continue
  | Sync
  | TouchStart PointerEvent
  | TouchEnd PointerEvent
  | NoOp

toDirection :: Arrows -> Direction
toDirection Arrows {..} =
  case (arrowX, arrowY) of
    (-1, 0) -> L
    (1, 0) -> R
    (0, -1) -> D
    (0, 1) -> U
    _ -> None

swipeDir :: (Ord b, Num b) => b -> b -> (b, b)
swipeDir xDiff yDiff
  | abs xDiff >= abs yDiff = (signum xDiff, 0)
  | otherwise = (0, signum yDiff)

swipe :: (Double, Double) -> (Double, Double) -> Action
swipe (px, py) (x, y) = GetArrows $ Arrows (round xDir) (round yDir)
  where
    xDiff = x - px
    yDiff = py - y
    (xDir, yDir) = swipeDir xDiff yDiff
