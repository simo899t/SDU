module Main where

import Data.List (intercalate)

-- Create an empty grid with spaces
createEmptyGrid :: Int -> Int -> [[Char]]
createEmptyGrid width height = 
    replicate height (replicate width ' ')

-- Add a frame around the grid
addFrameToGrid :: [[Char]] -> [[Char]]
addFrameToGrid grid =
    let height = length grid
        width = length (head grid)
        topBottom = '+' : replicate (width-2) '-' ++ ['+']
        
        addSides row = "|" ++ init (tail row) ++ "|"
        framedRows = map addSides grid
    in [topBottom] ++ framedRows ++ [topBottom]

-- Define dot patterns for each character (5x5 grid)
charPatterns :: Char -> [[Bool]]
charPatterns 'H' = [
    [True, False, False, False, True],
    [True, False, False, False, True],
    [True, True, True, True, True],
    [True, False, False, False, True],
    [True, False, False, False, True]]
charPatterns 'a' = [
    [False, True, True, True, False],
    [True, False, False, False, True],
    [True, True, True, True, True],
    [True, False, False, False, True],
    [True, False, False, True, True]]
charPatterns 'n' = [
    [True, True, True, True, False],
    [True, False, False, False, True],
    [True, False, False, False, True],
    [True, False, False, False, True],
    [True, False, False, False, True]]
charPatterns 'e' = [
    [True, True, True, True, True],
    [True, False, False, False, False],
    [True, True, True, True, False],
    [True, False, False, False, False],
    [True, True, True, True, True]]
charPatterns 'R' = [
    [True, True, True, True, False],
    [True, False, False, False, True],
    [True, True, True, True, False],
    [True, False, True, False, False],
    [True, False, False, True, False]]
charPatterns 'o' = [
    [False, True, True, True, False],
    [True, False, False, False, True],
    [True, False, False, False, True],
    [True, False, False, False, True],
    [False, True, True, True, False]]
charPatterns 't' = [
    [True, True, True, True, True],
    [False, False, True, False, False],
    [False, False, True, False, False],
    [False, False, True, False, False],
    [False, False, True, False, False]]
charPatterns _ = replicate 5 (replicate 5 False)  -- Default for unknown chars

-- Add dot text to the grid
addDotTextToGrid :: String -> String -> [[Char]] -> [[Char]]
addDotTextToGrid text1 text2 grid =
    let height = length grid
        width = length (head grid)
        
        -- Calculate positions for text
        textY1 = height `div` 2 - 3  -- Position for first text
        textY2 = height `div` 2 + 3  -- Position for second text
        
        -- Calculate starting X for centered text (each char is 5 wide with 1 space)
        textX1 = (width - (length text1 * 6 - 1)) `div` 2
        textX2 = (width - (length text2 * 6 - 1)) `div` 2
        
        -- Add first word
        grid1 = addDotWord textX1 textY1 text1 grid
        
        -- Add second word
        grid2 = addDotWord textX2 textY2 text2 grid1
        
    in grid2

-- Add a word using dot patterns
addDotWord :: Int -> Int -> String -> [[Char]] -> [[Char]]
addDotWord startX startY word grid =
    foldl (\ g (i, c) -> addDotChar (startX + i * 6) startY c g) grid (zip [0..] word)

-- Add a character using dot pattern
addDotChar :: Int -> Int -> Char -> [[Char]] -> [[Char]]
addDotChar x y char grid =
    let pat = charPatterns char
    in foldl (\ g (dy, row) -> 
            foldl (\ g' (dx, dot) -> 
                if dot then updateGrid (x + dx) (y + dy) '*' g' else g'
            ) g (zip [0..] row)
         ) grid (zip [0..] pat)

-- Update character at position (x,y) in the grid
updateGrid :: Int -> Int -> Char -> [[Char]] -> [[Char]]
updateGrid x y c grid
    | y < 0 || y >= length grid = grid
    | x < 0 || x >= length (grid !! y) = grid
    | otherwise = 
        let row = grid !! y
            newRow = take x row ++ [c] ++ drop (x+1) row
        in take y grid ++ [newRow] ++ drop (y+1) grid

-- Function to display the graph
textGraph :: IO ()
textGraph = do
    let 
        width = 60
        height = 30  -- Height to fit the dot text
        
        -- Create empty grid (no sine wave pattern)
        emptyGrid = createEmptyGrid width height
        
        -- Add the text message in dots
        textGrid = addDotTextToGrid "Hanne" "RotHe" emptyGrid
        
        -- Add frame
        framedGrid = addFrameToGrid textGrid
        
        -- Format as string
        result = intercalate "\n" framedGrid
    putStrLn result

-- Display a fake equation
showEquation :: IO ()
showEquation = do
    putStrLn "Din Mor"

main :: IO ()
main = do
    showEquation
    textGraph