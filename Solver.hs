module Solver where

import Data.List
import Data.Array
import Control.Monad.Logic
import Control.Monad
import Debug.Trace

data Square = F | G | U deriving (Eq, Show)
data Direction = H | V deriving (Eq, Show)
data Constraint = Constraint Problem Direction Int [Int]
data Problem    = Problem [Constraint] [Constraint] deriving (Show)
type Line = [Square]
type Index = (Int, Int)
type Board = Array Index Square

instance Show Constraint where
  show (Constraint _ d i cs) = "Constraint " ++ show d ++ " " ++ show i ++ " " ++ show cs

poss (Constraint (Problem _ cs) H i constraints) = poss' (length cs) constraints
poss (Constraint (Problem cs _) V i constraints) = poss' (length cs) constraints 
poss' :: Int -> [Int] -> [Line]
poss' i [] = [replicate i G]
poss' width all@(c:cs) 
  | width < (sum all + length all - 1) = error $ show width ++ " is not long enough for " ++ show all
  | otherwise = do
    gaps <- inits $ replicate (width - (sum all + length all - 1)) G
    remainder <- poss' (width - length gaps - length block) cs       
    return $ gaps ++ block ++ remainder
      where
        block = replicate c F ++ (if not (null cs) then [G] else []) -- TODO Less clumsy way of getting the last gap

possible :: Line -> Line -> Bool
possible = (and .) . zipWith match

-- possibleLine potentialLine matches
match F G = False
match G F = False
match _ _ = True

update :: Square -> Square -> Square
update x y | x == y || y == U = x
           | otherwise = error $ "Trying to update the known " ++ show y ++ " with " ++ show x ++ "."


findCommon :: [Line] -> Line
findCommon = foldl1 $ zipWith update'
  where update' x y | x == y = x
                    | otherwise = U
                   
line :: Constraint -> [Index]
line (Constraint (Problem _ cs) H i _) = [(x,i) | x <- [0..length cs - 1]]
line (Constraint (Problem cs _) V i _) = [(i,x) | x <- [0..length cs - 1]]

extractLine c board = map (board !) (line c)
updateLine :: Constraint -> Board -> Line -> Board
updateLine c board = (board //) . zip (line c)

solved :: Board -> Bool
solved = all (/=U) . elems

solveBoard :: Problem -> Board -> Board
solveBoard (Problem rows columns) b = foldl solveLine b (rows ++ columns)
solveLine :: Board -> Constraint -> Board
solveLine board constraint = updateLine constraint board newLine
                                   where
                                     oldLine = extractLine constraint board
                                     newLine = zipWith update (findCommon $ filter (possible oldLine) (poss constraint)) oldLine
                                     
emptyBoard (Problem rows columns) = listArray ((0,0),(length columns - 1,length rows - 1)) (repeat U)

solve problem = unsolvedBoards ++ [head solvedBoards]
  where
    (unsolvedBoards, solvedBoards) = span (not . solved) $ iterate (solveBoard problem) (emptyBoard problem)
      
-- | split at regular intervals
chunk :: Int -> [a] -> [[a]]
chunk _ [] = [[]]
chunk n xs = y1 : chunk n y2
  where
    (y1, y2) = splitAt n xs 

problem :: [[Int]] -> [[Int]] -> Problem
problem rows columns = let p = go p rows columns
                             in p
                         where go :: Problem -> [[Int]] -> [[Int]] -> Problem
                               go p rows columns = Problem rowConstraints columnConstraints
                                 where rowConstraints = zipWith (Constraint p H) [0..] rows       -- Going down
                                       columnConstraints = zipWith (Constraint p V) [0..] columns -- Going across

showBoard b = (unlines . (chunk $ (+1) $ snd $ snd $ bounds b) . map showSquare . elems) b

showSquare F = 'X'
showSquare G = ' '
showSquare U = '?'