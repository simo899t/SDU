module Game.MyGame (myGame) where

import Graphics.Gloss.Interface.Pure.Game
import Scratchy.Syntax

myGame :: SProg ()
myGame =
  -- Green snake (head at (10,15), tail extending left to (4,15))
  NewSprite (10,15) (Color green (circleSolid (cellSize * 0.6))) (\headG ->
  NewSprite (9,15)  (Color green (rectangleSolid cellSize cellSize)) (\tailG1 ->
  NewSprite (8,15)  (Color green (rectangleSolid cellSize cellSize)) (\tailG2 ->
  NewSprite (7,15)  (Color green (rectangleSolid cellSize cellSize)) (\tailG3 ->
  NewSprite (6,15)  (Color green (rectangleSolid cellSize cellSize)) (\tailG4 ->
  NewSprite (5,15)  (Color green (rectangleSolid cellSize cellSize)) (\tailG5 ->
  NewSprite (4,15)  (Color green (rectangleSolid cellSize cellSize)) (\tailG6 ->
  NewSprite (3,15)  (Color green (rectangleSolid cellSize cellSize)) (\tailG7 ->

  -- Blue snake (head at (20,15), tail extending left to (14,15))
  NewSprite (20,15) (Color blue (circleSolid (cellSize * 0.6))) (\headB ->
  NewSprite (21,15) (Color blue (rectangleSolid cellSize cellSize)) (\tailB1 ->
  NewSprite (22,15) (Color blue (rectangleSolid cellSize cellSize)) (\tailB2 ->
  NewSprite (23,15) (Color blue (rectangleSolid cellSize cellSize)) (\tailB3 ->
  NewSprite (24,15) (Color blue (rectangleSolid cellSize cellSize)) (\tailB4 ->
  NewSprite (25,15) (Color blue (rectangleSolid cellSize cellSize)) (\tailB5 ->
  NewSprite (26,15) (Color blue (rectangleSolid cellSize cellSize)) (\tailB6 ->
  NewSprite (26,15) (Color blue (rectangleSolid cellSize cellSize)) (\tailB7 ->

  -- Controls - these will set up the head movement AND ALL tail following
  player1SnakeControls headG [tailG1, tailG2, tailG3, tailG4, tailG5, tailG6, tailG7]
   `combine`
  player2SnakeControls headB [tailB1, tailB2, tailB3, tailB4, tailB5, tailB6, tailB7]))))))))))))))))


-- Move head in a direction, making first tail follow (and set up tail chain on first move)
moveInDirection :: SpritePtr -> [SpritePtr] -> Dir -> SProg ()
moveInDirection headPos tails dir =
  case tails of
    [] -> Pure ()
    (firstTail:_) ->
      setupTailChain tails `combine`
      OnTargetReached headPos (\curPos ->
        let nextPos = nextCell dir curPos in
        InspectCell nextPos (\cellResult ->
          case cellResult of
            HasBarrier -> SetBackgroundColor red (Pure ())
            HasSprite  -> SetBackgroundColor black (Pure ())
            IsFree     -> SetBackgroundColor white 
              (SetTarget headPos nextPos
                (SetTarget firstTail curPos (moveInDirection headPos tails dir))))
      ) (Pure ())

-- Following for tail segments (each tail follows the one in front)
setupTailChain :: [SpritePtr] -> SProg ()
setupTailChain [] = Pure ()
setupTailChain [_] = Pure ()
setupTailChain (leader:follower:rest) =
  followTail leader follower `combine` setupTailChain (follower:rest)

-- Each tail continuously follows the one in front
followTail :: SpritePtr -> SpritePtr -> SProg ()
followTail leader follower =
  OnTargetReached leader (\leaderPos ->
    SetTarget follower leaderPos (Pure ())
  ) (Pure ())


-- Player 1 controls: start moving head in a direction
player1SnakeControls :: SpritePtr -> [SpritePtr] -> SProg ()
player1SnakeControls headG tails =
  OnKeyEvent (Char 'w') (moveInDirection headG tails U) 
  (OnKeyEvent (Char 's') (moveInDirection headG tails D) 
  (OnKeyEvent (Char 'd') (moveInDirection headG tails R) 
  (OnKeyEvent (Char 'a') (moveInDirection headG tails L) 
  (Pure ()))))

-- Player 2 controls: start moving head in a direction
player2SnakeControls :: SpritePtr -> [SpritePtr] -> SProg ()
player2SnakeControls headB tails =
  OnKeyEvent (Char 'i') (moveInDirection headB tails U)
  (OnKeyEvent (Char 'k') (moveInDirection headB tails D) 
  (OnKeyEvent (Char 'l') (moveInDirection headB tails R) 
  (OnKeyEvent (Char 'j') (moveInDirection headB tails L) 
  (Pure ()))))
