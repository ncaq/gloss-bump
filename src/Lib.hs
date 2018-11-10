{-# LANGUAGE NamedFieldPuns #-}
module Lib
    ( someFunc
    ) where

import           Data.List
import           Graphics.Gloss
import           Graphics.Gloss.Data.Picture
import           Graphics.Gloss.Data.ViewPort
import           System.Random

newtype World
  = World
    { cars :: [Car]
    }
    deriving (Eq, Ord, Show, Read)

data Car
  = Car
    { carX     :: Float
    , carY     :: Float
    , carAngle :: (Float, Float)
    } deriving (Eq, Ord, Show, Read)

someFunc :: IO ()
someFunc = do
  xs  <- map (\a -> a - 0.5) . take 100 . randoms <$> newStdGen
  ys  <- map (\a -> a - 0.5) . take 100 . randoms <$> newStdGen
  axs <- map (\a -> a - 0.5) . take 100 . randoms <$> newStdGen
  ays <- map (\a -> a - 0.5) . take 100 . randoms <$> newStdGen
  simulate FullScreen white 100 (World $ (\(x, y, ax, ay) -> Car x y (ax, ay)) <$>
                                 zip4 xs ys axs ays) render worldNext

render :: World -> Picture
render world = Pictures $ renderCar <$> cars world

renderCar :: Car -> Picture
renderCar Car{carX, carY} = translate carX carY $ circle carSize

carSize = 10

worldNext :: ViewPort -> Float -> World -> World
worldNext _ _ world = World $
  (\Car{carX, carY, carAngle} ->
      Car{carX = carX + fst carAngle, carY = carY + snd carAngle, carAngle}) <$>
  cars world
