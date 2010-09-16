module Solver where

import Data.Ord (comparing)
import Data.List
import Data.Array
import Data.Function
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
        block = replicate c F ++ [G | not (null cs)]

possible :: Line -> Line -> Bool
possible = (and .) . zipWith match

possibleLines :: Board -> Constraint -> [Line]
possibleLines b c = filter (possible $ extractLine c b) (poss c)

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

solvable :: Problem -> Board -> Bool
solvable (Problem rows columns) b = all (solvableLine b) (rows ++ columns)
solvableLine :: Board -> Constraint -> Bool
solvableLine board constraint = (not . null . filter (possible $ extractLine constraint board)) (poss constraint)

guess :: Problem -> Board -> [Board]
guess (Problem rows columns) b = do 
  constraint <- rows ++ columns
  line <- filter ((> 1) . length) $ sortBy (comparing length) $ possibleLines b constraint
  return $ updateLine constraint b line

propagateConstraints :: Problem -> Board -> Board
propagateConstraints (Problem rows columns) b = foldl propagateThroughLine b (rows ++ columns)
propagateThroughLine :: Board -> Constraint -> Board
propagateThroughLine board constraint = updateLine constraint board newLine
                                   where
                                     oldLine = extractLine constraint board
                                     newLine = zipWith update (findCommon $ possibleLines board constraint) oldLine
                                     
emptyBoard (Problem rows columns) = listArray ((0,0),(length columns - 1,length rows - 1)) (repeat U)

solve problem = searchSolve problem $ emptyBoard problem

searchSolve problem board | solved $ last constraintBoards = constraintBoards
                          | otherwise = constraintBoards ++ searchBoards
                            where
                              constraintBoards = constraintSolve problem board
                              searchBoards = head $ filter (solved . last) $ map (searchSolve problem) $ guess problem $ last constraintBoards

constraintSolve problem = takeUntilDuplicate . iterate (propagateConstraints problem)
               
takeUntilDuplicate :: Eq a => [a] -> [a]
takeUntilDuplicate [] = []
takeUntilDuplicate [x] = [x]
takeUntilDuplicate (x:y:xs) | x == y = [x]
                            | otherwise = x : takeUntilDuplicate(y:xs)
  
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

showBoard b = (unlines . chunk ((+1) $ snd $ snd $ bounds b) . map showSquare . elems) b

showSquare F = 'X'
showSquare G = ' '
showSquare U = '?'