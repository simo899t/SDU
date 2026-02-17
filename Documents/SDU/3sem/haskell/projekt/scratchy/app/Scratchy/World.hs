module Scratchy.World where

import Data.List (partition)
import Control.Monad
import Graphics.Gloss.Interface.Pure.Game
--import Graphics.Gloss (makeColorI)

import Scratchy.Syntax
--import Scratchy.Board (drawSnakeIshBoard)
--import Scratchy (drawSprite)

-- Some useful helpers
------------------------------------------------------------

imapM :: Applicative m => (Int -> a -> m b) -> [a] -> m [b]
imapM f = zipWithM f [0..]

imapM_ :: Applicative m => (Int -> a -> m b) -> [a] -> m ()
imapM_ f xs = () <$ zipWithM f [0..] xs

setAt :: Int -> a -> [a] -> [a]
setAt i x xs = zipWith (\j y -> if i == j then x else y) [0..] xs

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)


-- Sprites
------------------------------------------------------------

data Sprite = Sprite
  { pic  :: Picture
  , pos  :: Pos
  , tgt  :: Cell }
  deriving (Show , Eq)


-- World
------------------------------------------------------------

data World = World
  { bg        :: Color
  , sprites   :: ![Sprite]
  , keysHeld  :: ![Key]
  , prog      :: !(SProg ())        -- Current program
  , keyHdlrs  :: ![(Key, SProg ())] -- Key pressed
  , trHdlrs   :: ![( SpritePtr
                   , Cell
                     -> SProg () )] -- Target reached
  , tuHdlrs   :: ![( SpritePtr
                   , Cell
                     -> Cell
                     -> SProg () )] -- Target updated
  , bhHdlrs   :: ![( SpritePtr
                   , Cell
                     -> Cell
                     -> SProg () )] -- Barrier hit
  , timers    :: ![(Duration, SProg ())] -- Timers

  }


-- Game
------------------------------------------------------------

newtype Game a = Game { runGame :: World -> (World, a) }
  deriving Functor

instance Applicative Game where
  pure x = Game $ \w -> (w,x)
  (<*>) = ap

instance Monad Game where
  Game f >>= k = Game $ \w ->
    let (w', x) = f w in runGame (k x) w'


-- Sprite movement ops
------------------------------------------------------------

moveSprites :: Float -> Game ()
moveSprites dt = Game $ \w@World{..} ->
  let (w', sprites') = flip runGame w $ imapM (moveSprite dt) sprites
  in ( w' { sprites = sprites' }
     , () )

moveSprite :: Float -> SpritePtr -> Sprite -> Game Sprite
moveSprite dt ptr m@Sprite{..} = Game $ \w@World{..} ->
  let (tx,ty) = tgt
      (px,py) = pos
      dx = fromIntegral tx - px
      dy = fromIntegral ty - py
      dist = sqrt (dx*dx + dy*dy)
      stepSize = cellsPerSec * dt
  in if dist <= stepSize * dt
     then ( w { prog
                = prog `combine`
                (case lookup ptr trHdlrs of
                   Just f -> f tgt
                   Nothing -> Pure ()) }
          , m { pos = (fromIntegral tx, fromIntegral ty) } )
     else ( w
          , m { pos = ( px + (stepSize/dist)*dx
                      , py + (stepSize/dist)*dy ) } )

-- not used???
setSpriteTarget :: SpritePtr -> Cell -> Game ()
setSpriteTarget ptr c = Game $ \w@World{..} ->
  let spr = sprites !! ptr in
  if inBounds c
  then ( w { sprites
             = setAt ptr
                 (spr { tgt = c })
                 sprites
           , prog
             = prog `combine`
               (case lookup ptr tuHdlrs of
                  Just f -> f (tgt spr) c
                  Nothing -> Pure ()) }
       , () )
  else ( w { prog
             = prog `combine`
               (case lookup ptr bhHdlrs of
                  Just f -> f (tgt spr) c
                  Nothing -> Pure ()) }
       , () )

spriteExistsAt :: Cell -> [Sprite] -> Maybe SpritePtr
spriteExistsAt c = go . zip [0..]
  where
    go [] = Nothing
    go ((p,s):sprs) =
      let (p1, p2) = pos s
          c'       = (round p1, round p2)
      in if c == c' || c == tgt s then Just p else go sprs

-- Input handling
------------------------------------------------------------

handleEvent :: Event -> Game ()
handleEvent (EventKey c d _ _) = case d of
  Down -> press c
  Up   -> release c
handleEvent _ = pure ()

press :: Key -> Game ()
press d = Game $ \w@World{..} ->
  ( w { keysHeld = d : filter (/= d) keysHeld }
  , () )

release :: Key -> Game ()
release d = Game $ \w@World{..} ->
  ( w { keysHeld = filter (/= d) keysHeld }
  , () )

handleKeyEvents :: Game ()
handleKeyEvents = Game (\w@World{..} ->
  runGame (mapM_ handleKey keysHeld) w)

handleKey :: Key -> Game ()
handleKey k = Game $ \w@World{..} ->
  ( w { prog
        = prog `combine`
          (case lookup k keyHdlrs of
             Just m -> m
             Nothing -> Pure ()) }
  , () )


-- Timers
------------------------------------------------------------

tickTimers :: Game ()
tickTimers = Game $ \w@World{..} ->
  let ts = map (\(d,m) -> (d-1,m)) timers
      (done,remaining) = partition (\(d,_) -> d <= 0) ts
      p' = foldr (\(_,m) -> combine m) (Pure ()) done
  in ( w { timers = remaining
         , prog = prog `combine` p' }
     , () )


-- Running programs
------------------------------------------------------------

runProg :: Game ()
runProg = Game (\world -> (go (prog world) world { prog = Pure () }, ()))
  where
    go :: SProg () -> World -> World
    go (Pure ()) world = world -- the pure function means do nothing, so if the program is pure it should just do nothing
    go (Seq program1 program2) world =
      let worldAfterProgram1 = go program1 world -- applies the first program to the world
      in go program2 worldAfterProgram1
    go (OnKeyEvent key action rest) world =
      go rest world { keyHdlrs = (key, action) : keyHdlrs world }
    go (OnTargetReached ptr handler rest) world =
      go rest world { trHdlrs = (ptr, handler) : trHdlrs world }
    go (OnTargetUpdated ptr handler rest) world =
      go rest world { tuHdlrs = (ptr, handler) : tuHdlrs world }
    go (OnBarrierHit ptr handler rest) world =
      go rest world { bhHdlrs = (ptr, handler) : bhHdlrs world }
    go (NewSprite cell pic next) world =
      let newSprite = Sprite pic (fromIntegral (fst cell), fromIntegral (snd cell)) cell
          spriteIndex = length (sprites world)
          worldWithNewSprite = world { sprites = sprites world ++ [newSprite] }
      in go (next spriteIndex) worldWithNewSprite
    go (SetColor ptr color rest) world =
      let allSprites = sprites world
          oldSprite = allSprites !! ptr
          newSprite = oldSprite { pic = color `Color` pic oldSprite }
          updatedSprites = setAt ptr newSprite allSprites
      in go rest world { sprites = updatedSprites }
    go (SetTarget ptr cell rest) world =
      let allSprites = sprites world
          oldSprite = allSprites !! ptr
          updatedSprite = oldSprite { tgt = cell }
          updatedSprites = setAt ptr updatedSprite allSprites
      in go rest world { sprites = updatedSprites }
    go (GetTarget ptr next) world =
      let sprite = sprites world !! ptr
      in go (next (tgt sprite)) world
    go (SetBackgroundColor color rest) world =
      go rest world { bg = color }
    go (InspectCell cell next) world =
      let hasSprite = spriteExistsAt cell (sprites world) /= Nothing
          hasBarrier = not (inBounds cell) 
          result
            | hasBarrier = HasBarrier
            | hasSprite  = HasSprite
            | otherwise  = IsFree
      in go (next result) world
    go (After duration action rest) world =
      go rest world { timers = timers world ++ [(duration, action)] }

