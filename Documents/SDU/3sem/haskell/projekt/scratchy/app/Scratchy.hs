module Scratchy where

import Graphics.Gloss.Interface.Pure.Game

import Scratchy.World
import Scratchy.Syntax

import Game.MyGame (myGame)

-- Main
------------------------------------------------------------

main :: IO ()
main = play
  (InWindow "Snakey game ß)" (gridW * round cellSize, gridH * round cellSize) (100, 100))
  white
  120
  (World
    white
    []
    []
    myGame
    []
    []
    []
    []
    []
  )
  drawWorld
  (\e -> fst . runGame (handleEvent e))
  (\tick ->
     fst . runGame ( runProg
                     >> moveSprites tick
                     >> handleKeyEvents
                     >> tickTimers
                     >> runProg
                   ) )


-- Rendering
------------------------------------------------------------

drawWorld :: World -> Picture
drawWorld World{..} =
  Pictures
    ( Color bg (rectangleSolid (fromIntegral gridW * cellSize) (fromIntegral gridH * cellSize))
    : drawGrid
    : map drawSprite sprites )

drawGrid :: Picture
drawGrid =
  let halfW = fromIntegral gridW * cellSize / 2
      halfH = fromIntegral gridH * cellSize / 2
      xs = [(-halfW), (-halfW + cellSize) .. halfW]
      ys = [(-halfH), (-halfH + cellSize) .. halfH]
      vlines = [ Line [ (x, -halfH), (x,  halfH) ] | x <- xs ]
      hlines = [ Line [ (-halfW, y), ( halfW, y) ] | y <- ys ]
  in Color (greyN 0.85) (Pictures (vlines ++ hlines))

drawSprite :: Sprite -> Picture
drawSprite Sprite{..} =
  let (x, y) = toScreen pos
  in Translate x y $ pic

toScreen :: (Float, Float) -> (Float, Float)
toScreen (gx, gy) =
  let fx = gx - (fromIntegral gridW - 1) / 2
      fy = gy - (fromIntegral gridH - 1) / 2
  in (fx * cellSize, fy * cellSize)


