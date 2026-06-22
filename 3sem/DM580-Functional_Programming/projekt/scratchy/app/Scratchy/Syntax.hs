module Scratchy.Syntax where

import Graphics.Gloss.Interface.Pure.Game

-- Types
------------------------------------------------------------

type Duration  = Int
type Pos       = (Float, Float)
type Cell      = (Int, Int)
type SpritePtr = Int


-- Constants
------------------------------------------------------------

cellsPerSec :: Float
cellsPerSec = 5

gridW, gridH :: Int
gridW = 30
gridH = 30

inBounds :: (Int,Int) -> Bool
inBounds (x,y) = x >= 0 && x < gridW && y >= 0 && y < gridH

cellSize :: Float
cellSize = 30

winW, winH :: Int
winW = round (fromIntegral gridW * cellSize)
winH = round (fromIntegral gridH * cellSize)


-- Direction
------------------------------------------------------------

data Dir = U | D | L | R deriving (Eq,Ord,Show)

dirVec :: Dir -> (Int, Int)
dirVec U = (0, 1)
dirVec D = (0,-1)
dirVec L = (-1,0)
dirVec R = (1, 0)

nextCell :: Dir -> (Int, Int) -> (Int, Int)
nextCell d (cx,cy) =
  let (dx,dy) = dirVec d
  in (cx + dx, cy + dy)


-- Syntax
------------------------------------------------------------

data InspectionResult
  = HasBarrier | HasSprite | IsFree

data SProg a =
  Pure a
    -- Does nothing and returns the world unchanged.

  | Seq (SProg ()) (SProg a)
    -- Runs the first program 'SProg ()', then runs 'SProg a' on the updated world.

  | OnKeyEvent Key (SProg ()) (SProg a)
    -- Registers a handler so that when key 'Key' is pressed, program 'SProg ()' will be run.
    -- Continues interpreting the rest '(SProg a)' of the program.

  | OnTargetReached SpritePtr (Cell -> SProg ()) (SProg a)
    -- Registers a handler for when the sprite at 'SpritePtr' reaches its target cell.
    -- Runs 'Cell -> SProg ()' with that sprite.
    -- Continues with the rest of the program '(SProg a)'.

  | OnTargetUpdated SpritePtr (Cell -> Cell -> SProg ()) (SProg a)
    -- Registers a handler for when the sprite at 'SpritePtr' has its target updated.
    -- Runs 'Cell -> Cell -> SProg ()' with the relevant cells.
    -- Continues with the rest of the program '(SProg a)'.

  | OnBarrierHit SpritePtr (Cell -> Cell -> SProg ()) (SProg a)
    -- Registers a handler for when the sprite at 'SpritePtr' tries to move into a barrier (out of bounds).
    -- Runs 'Cell -> Cell -> SProg ()' with the relevant cells.
    -- Continues with the rest of the program '(SProg a)'.

  | NewSprite Cell Picture (SpritePtr -> SProg a)
    -- Creates a new sprite at the given cell 'Cell' with the given picture 'Picture'.
    -- Runs '(SpritePtr -> SProg a)' with the new sprite's pointer.

  | SetColor SpritePtr Color (SProg a)
    -- Sets the color of the sprite at 'SpritePtr' to 'Color'.
    -- Continues by running 'SProg a'.

  | SetTarget SpritePtr Cell (SProg a)
    -- Sets the target of the sprite at 'SpritePtr' to the cell 'Cell'.
    -- Continues by running 'SProg a'.

  | GetTarget SpritePtr (Cell -> SProg a)
    -- Gets the target of the sprite at 'SpritePtr'.
    -- Runs 'Cell -> SProg a' with the target cell.

  | SetBackgroundColor Color (SProg a)
    -- Sets the background color of the window to 'Color'.
    -- Continues by running 'SProg a'.

  | InspectCell Cell (InspectionResult -> SProg a)
    -- Checks the state of the cell (whether it contains a sprite, is in bounds, or is free).
    -- Continues by running 'InspectionResult -> SProg a' with the result.

  | After Duration (SProg ()) (SProg a)
    -- Runs the program 'SProg ()' after 'Duration' ticks.
    -- Continues with 'SProg a' as the rest.

  deriving Functor


-- Combining programs
------------------------------------------------------------

combine :: SProg () -> SProg () -> SProg ()
combine = Seq




