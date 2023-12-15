{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use all" #-}
{-# HLINT ignore "Use !!" #-}
{-# HLINT ignore "Use concat" #-}
{-# HLINT ignore "Fuse foldr/map" #-}
{-# HLINT ignore "Use list comprehension" #-}

module GameLogic where
import Data.List ( transpose )


emptyBoard :: [[Int]]
emptyBoard = [[0, 0, 0, 0],
              [0, 0, 0, 0],
              [0, 0, 0, 0],
              [0, 0, 0, 0]]


-- Random Board generation functions
generateRandomBoard :: [Double] -> [[Int]]
generateRandomBoard selectedIndices = do
                          let possibleIndices = [0..15]
                              firstVal = floor (head selectedIndices * 16)
                              nextIndices = foldr (++) [] (map (\x -> if x == firstVal then [] else [x]) possibleIndices)
                              secondIndex = floor (head (drop 1 selectedIndices) * 15)
                              secondVal = atIndex nextIndices secondIndex
                              vals = [firstVal, secondVal]
                              boardVals = map (\x -> if x `elem` vals then 2 else 0) possibleIndices
                              finalBoard = [take 4 boardVals, take 4 $ drop 4 boardVals, take 4 $ drop 8 boardVals, take 4 $ drop 12 boardVals]
                          finalBoard


-- Random tile generation functions

addTile :: [[Int]] -> [Double] -> [[Int]]
addTile board rNums = finalBoard
  where
    tileVal = generateRandomTile (take 1 rNums)
    tileLocation = generateRandomTileLoc (zeroIndices (flatten board)) (take 1 (drop 1 rNums))
    finalBoard = insertTile board tileLocation tileVal

generateRandomTile :: [Double] -> Int
generateRandomTile vals = do
                          let val = head vals
                              v = if val >= 0.9 then 4 else 2
                          v

generateRandomTileLoc :: [Int] -> [Double] -> Int
generateRandomTileLoc indices vals = atIndex indices val
    where
      val = floor (head vals * fromIntegral (length indices)) :: Int


insertTile :: [[Int]] -> Int -> Int -> [[Int]]
insertTile board loc val = [take 4 boardVals, take 4 $ drop 4 boardVals, take 4 $ drop 8 boardVals, take 4 $ drop 12 boardVals]
    where
      helper [] _ = []
      helper (_:xs) 0 = val : xs
      helper (x:xs) n = x : helper xs (n - 1)
      boardVals = helper (flatten board) loc


-- Board sliding and merging functions

shiftDown :: [[Int]] -> ([[Int]], Int)
shiftDown board = (transpose (reverseRows b), score)
    where
      (b, score) = mergeRows (shiftRows (reverseRows (transpose board)))

shiftUp :: [[Int]] -> ([[Int]], Int)
shiftUp board = (transpose b, score)
    where
      (b, score) = mergeRows (shiftRows (transpose board))

shiftLeft :: [[Int]] -> ([[Int]], Int)
shiftLeft board = mergeRows (shiftRows board)

shiftRight :: [[Int]] -> ([[Int]], Int)
shiftRight board = (reverseRows b, score)
    where
      (b, score) = mergeRows (shiftRows (reverseRows board))


mergeRows :: [[Int]] -> ([[Int]], Int)
mergeRows board = (newBoard, score)
  where
    mergeRow :: Int -> [Int] -> ([Int], Int)
    mergeRow 0 _ = ([], 0)
    mergeRow n [] = (replicate n 0, 0)
    mergeRow n [x] = (x : replicate (n - 1) 0, 0)
    mergeRow n (x:y:xs)
      | x == y = ((x + y) : eq_b, x + y + eq_s)
      | otherwise = (x : neq_b, neq_s)
        where
          (eq_b, eq_s) = mergeRow (n - 1) xs
          (neq_b, neq_s) = mergeRow (n - 1) (y:xs)
    mergedOutput = map (mergeRow 4) board
    score = listSum (map snd mergedOutput)
    newBoard = map fst mergedOutput


-- Board sliding and merging helper functions
numZeros :: [Int] -> Int
numZeros r = length (filter (== 0) r)

shiftRows :: [[Int]] -> [[Int]]
shiftRows board = map shiftRow board
  where
      shiftRow :: [Int] -> [Int]
      shiftRow row = filter (/= 0) row ++ replicate (numZeros row) 0

reverseRows :: [[Int]] -> [[Int]]
reverseRows = map reverse

listSum :: [Int] -> Int
listSum xs = sum xs


flatten :: [[Int]] -> [Int]
flatten board = concat board

zeroIndices :: [Int] -> [Int]
zeroIndices b = helper b 0
    where
      helper [] _ = []
      helper (x:xs) num = if x == 0 then num : helper xs (num + 1) else helper xs (num + 1)

atIndex :: [Int] -> Int -> Int
atIndex [] _ = 0
atIndex (x:_) 0 = x
atIndex (_:xs) n = atIndex xs (n-1)



-- Game over checking functions

isGameOver :: [[Int]] -> Int -> Bool
isGameOver board bombs = (not (hasZeros board)) && not (hasConsecutiveTiles board || hasConsecutiveTiles (transpose board)) && bombs == 0

hasConsecutiveTiles :: [[Int]] -> Bool
hasConsecutiveTiles board = foldr (||) False (map helper board)
    where
        helper [e1, e2, e3, e4] = (e1 == e2) || (e2 == e3) || (e3 == e4)

hasZeros :: [[Int]] -> Bool
hasZeros board = elem 0 (flatten board)


-- Bomb feature functions
removeFromBoard :: Int -> [[Int]] -> ([[Int]], Bool)
removeFromBoard n gameBoard =
    let removeCellFromBoard row = map (\x -> if x == n then 0 else x) row
        bombedBoard = map removeCellFromBoard gameBoard
        isEmpty = bombedBoard == emptyBoard
    in if isEmpty then (gameBoard, isEmpty) else (bombedBoard, isEmpty)