module Lib where

data BinaryTree = BinaryTree {leftSubTree::Maybe BinaryTree, rightSubTree:: Maybe BinaryTree} | Leaf Char Float deriving (Show, Eq)

data Bit = Bit {v::Int}  deriving Show

getValuesBits:: [Bit] -> [Int]
getValuesBits bs = map v bs

-- true links    false rechts
mergeTrees:: BinaryTree -> BinaryTree -> BinaryTree
mergeTrees b1 b2 = BinaryTree (Just b1) (Just b2)


frequencyTree:: Maybe BinaryTree -> Float
frequencyTree Nothing = 0
frequencyTree (Just (Leaf _ f)) = f
frequencyTree (Just(BinaryTree l r)) = frequencyTree l + frequencyTree r

depthTree:: Maybe BinaryTree -> Int
depthTree Nothing =  0
depthTree (Just (Leaf _ _)) = 0
depthTree (Just (BinaryTree left right)) = 1 + (max (depthTree left) (depthTree right))

getChars:: Maybe BinaryTree -> String
getChars Nothing = []
getChars (Just (Leaf c _ )) = [c]
getChars (Just (BinaryTree l r)) = (getChars l) ++ (getChars r)

getTreeLowestFreq:: BinaryTree -> BinaryTree -> BinaryTree
getTreeLowestFreq b1 b2 = if frequencyTree (Just b1) < frequencyTree (Just b2) then b1 else b2


getLowestFreq:: [BinaryTree] -> Float
getLowestFreq  [b] = frequencyTree (Just b)
getLowestFreq (b:bs) = min (getLowestFreq [b]) (getLowestFreq bs)

getTreesLowestFreq:: [BinaryTree] -> [BinaryTree]
getTreesLowestFreq bs = let lowestFreq = getLowestFreq bs in filter (\x -> (frequencyTree (Just x)) == lowestFreq) bs

getLowestDepth:: [BinaryTree] -> Int
getLowestDepth bs =  minimum (map (\x -> depthTree (Just x) ) bs)

getTreeLowestDepth :: [BinaryTree] -> BinaryTree
getTreeLowestDepth bs = head ( filter (\x -> (depthTree (Just x)) == (getLowestDepth bs))   bs)


getNextTree:: [BinaryTree] -> (BinaryTree, [BinaryTree])
getNextTree bs = 
  let next = getTreeLowestDepth  (getTreesLowestFreq bs) 
  in (next, filter (\x -> x /= next) bs)

getTwoNextTrees:: [BinaryTree] -> ((BinaryTree, BinaryTree ), [BinaryTree])
getTwoNextTrees bs = ((first, second), bs3)
  where
  (first, bs2) = getNextTree bs
  (second, bs3) = getNextTree bs2


alphabet:: String
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

compressTreeForText:: String -> BinaryTree
compressTreeForText text = buildCompressingTree startTrees
  where
  frequency_alphabet = frequency text alphabet
  rFreq = relativeFrequency frequency_alphabet
  startTrees = getStartTrees rFreq

buildCompressingTree:: [BinaryTree] -> BinaryTree
buildCompressingTree [b] = b
buildCompressingTree bs = buildCompressingTree (bs2 ++ [newTree])
  where
  ((first, second), bs2) = getTwoNextTrees bs
  newTree = mergeTrees first second

compressChar:: Maybe BinaryTree -> Char -> [Bit]
compressChar Nothing _ = []
compressChar (Just (Leaf leafChar _ )) c = []
compressChar (Just (BinaryTree l r)) c = if (elem c charL) then [Bit 0] ++ compressChar l c else [Bit 1] ++ compressChar r c
  where
  charL = getChars l 
  charR = getChars r

compressText:: Maybe BinaryTree -> String -> [Bit]
compressText b ss = concat (map (\x -> compressChar b x) ss)

getStartTrees:: [(Char, Float)] -> [BinaryTree]
getStartTrees freqs = map (\tuple -> Leaf (fst tuple) (snd tuple)) freqs


frequency:: [Char] -> [Char] -> [(Char,Int)]
frequency text alphabet = zip alphabet (map (\char -> countAmountEl text char) alphabet)

relativeFrequency:: [(Char, Int)] -> [(Char, Float)]
relativeFrequency freq = let summ =sum ( map (\x -> snd x) freq) in map (\tuple -> ( fst tuple,  ((fromIntegral (snd tuple)) / (fromIntegral summ)) * 100 ) ) freq

countAmountEl:: [Char] -> Char -> Int
countAmountEl list element = length (filter (\x -> x == element) list)
