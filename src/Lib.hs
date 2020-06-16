module Lib where

import Data.List (group, intersperse, intercalate, elemIndex)
import Data.Char

-- BinaryTree wenn Blatt dann Char(Buchstabe) und Float(Häufigkeit)
data BinaryTree = BinaryTree {leftSubTree::Maybe BinaryTree, rightSubTree:: Maybe BinaryTree} | Leaf Char Float deriving (Show, Eq)

-- Bit 0 oder 1
data Bit = Bit {v::Int}  deriving (Show, Eq) -- 0 1
-- gibt Werte der Bits zurück
getValuesBits:: [Bit] -> [Int]
getValuesBits bs = map v bs

-- true links    false rechts
mergeTrees:: BinaryTree -> BinaryTree -> BinaryTree
mergeTrees b1 b2 = BinaryTree (Just b1) (Just b2)

-- berechnet die Häufigkeit summiert von allen Blättern
frequencyTree:: Maybe BinaryTree -> Float
frequencyTree Nothing = 0
frequencyTree (Just (Leaf _ f)) = f
frequencyTree (Just(BinaryTree l r)) = frequencyTree l + frequencyTree r

-- berechnet die Tiefe eines Baums
depthTree:: Maybe BinaryTree -> Int
depthTree Nothing =  0
depthTree (Just (Leaf _ _)) = 0
depthTree (Just (BinaryTree left right)) = 1 + (max (depthTree left) (depthTree right))

-- gibt alle Buchstaben der Blätter zurück
getChars:: Maybe BinaryTree -> String
getChars Nothing = []
getChars (Just (Leaf c _ )) = [c]
getChars (Just (BinaryTree l r)) = (getChars l) ++ (getChars r)

-- gibt von 2 Bäumen, den Baum mit der niedrigsten Frequenz zurück
getTreeLowestFreq:: BinaryTree -> BinaryTree -> BinaryTree
getTreeLowestFreq b1 b2 = if frequencyTree (Just b1) < frequencyTree (Just b2) then b1 else b2

-- gibt von einer Liste von Bäumen, die kleinste Häufigkeit zurück
getLowestFreq:: [BinaryTree] -> Float
getLowestFreq  [b] = frequencyTree (Just b)
getLowestFreq (b:bs) = min (getLowestFreq [b]) (getLowestFreq bs)

-- Gibt von einer Liste an Bäumen, den Baum mit der niedrigsten Frequenz zurück
getTreesLowestFreq:: [BinaryTree] -> [BinaryTree]
getTreesLowestFreq bs = let lowestFreq = getLowestFreq bs in filter (\x -> (frequencyTree (Just x)) == lowestFreq) bs

-- gibt die niedrigste Tiefe der Bäume zurück
getLowestDepth:: [BinaryTree] -> Int
getLowestDepth bs =  minimum (map (\x -> depthTree (Just x) ) bs)

-- Gibt von einer Liste an Bäumen den Baum mit der niedrigsten Tiefe zurück
getTreeLowestDepth :: [BinaryTree] -> BinaryTree
getTreeLowestDepth bs = head ( filter (\x -> (depthTree (Just x)) == (getLowestDepth bs))   bs)

-- gibt den für die Huffman Codierung nächsten Baum zurück und eine Liste der Bäume ohne diesen.
getNextTree:: [BinaryTree] -> (BinaryTree, [BinaryTree])
getNextTree bs = 
  let next = getTreeLowestDepth  (getTreesLowestFreq bs) 
  in (next, filter (\x -> x /= next) bs)

-- gibt die 2 nächsten Bäume für die Huffman Codierung zurück
getTwoNextTrees:: [BinaryTree] -> ((BinaryTree, BinaryTree ), [BinaryTree])
getTwoNextTrees bs = ((first, second), bs3)
  where
  (first, bs2) = getNextTree bs
  (second, bs3) = getNextTree bs2

-- Das Alphabet Groß
alphabet:: String
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

digits:: String
digits = "0123456789"

getUniques:: String -> String
getUniques text = getUniques' text []

getUniques':: String -> String -> String
getUniques' [] uniques = uniques
getUniques' (x:text) uniques = if (elem x uniques) then getUniques' text uniques else getUniques' text ([x] ++ uniques)

getTextsAlphabet:: String -> String
getTextsAlphabet text = getUniques text


-- gibt für einen Text, den Baum für die Huffmann Codierung zurück
compressTreeForText:: String -> BinaryTree
compressTreeForText text = buildCompressingTree startTrees
  where
  frequency_alphabet = frequency text (getTextsAlphabet text)
  rFreq = relativeFrequency frequency_alphabet
  startTrees = getStartTrees rFreq


-- baut aus einer Liste an Bäumen den Huffman COdierungs Baum
buildCompressingTree:: [BinaryTree] -> BinaryTree
buildCompressingTree [b] = b
buildCompressingTree bs = buildCompressingTree (bs2 ++ [newTree])
  where
  ((first, second), bs2) = getTwoNextTrees bs
  newTree = mergeTrees first second


-- "verschlüsselt"  einen Buchstabe mit einem Baum
compressChar:: Maybe BinaryTree -> Char -> [Bit]
compressChar Nothing _ = []
compressChar (Just (Leaf leafChar _ )) c = []
compressChar (Just (BinaryTree l r)) c = if (elem c charL) then [Bit 0] ++ compressChar l c else [Bit 1] ++ compressChar r c
  where
  charL = getChars l 
  charR = getChars r

-- "entschlüsselt" Bits zum Buchstabe
deCompressChar:: Maybe BinaryTree -> [Bit] -> (Char, [Bit])
deCompressChar Nothing  bs= ('a', [])
deCompressChar (Just (Leaf c f)) bs = (c, bs)
deCompressChar (Just (BinaryTree l r)) (b:bs) = if (b == Bit 0) then deCompressChar l bs  else deCompressChar r bs

-- "entschlüsselt" einen Text mit einem Baum
deCompressText:: Maybe BinaryTree -> [Bit] -> String
deCompressText t [] = []
deCompressText t bs = [c] ++ (deCompressText t bs2)
  where
  (c, bs2) = deCompressChar t bs

-- "verschlüsselt" einen Text mit dem Huffmann Baum
compressText:: Maybe BinaryTree -> String -> [Bit]
compressText b ss = concat (map (\x -> compressChar b x) ss)

-- erstellt aus einer Liste an Häufigkeiten für Buchstaben, die Bäume mit diesem Buchstabe Häufigkeit
getStartTrees:: [(Char, Float)] -> [BinaryTree]
getStartTrees freqs = map (\tuple -> Leaf (fst tuple) (snd tuple)) freqs

-- berechnet die absolute Häufigkeit für Buchstaben in einem Text
frequency:: [Char] -> [Char] -> [(Char,Int)]
frequency text alphabet = zip alphabet (map (\char -> countAmountEl text char) alphabet)

-- die relative Häufigkeit aus der absoluten Häufigkeit
relativeFrequency:: [(Char, Int)] -> [(Char, Float)]
relativeFrequency freq = let summ =sum ( map (\x -> snd x) freq) in map (\tuple -> ( fst tuple,  ((fromIntegral (snd tuple)) / (fromIntegral summ)) * 100 ) ) freq

--  zählt wie oft ein Element ein einer Liste vorkommt
countAmountEl:: [Char] -> Char -> Int
countAmountEl list element = length (filter (\x -> x == element) list)


unJust:: Maybe Int -> Int
unJust Nothing = -1
unJust (Just a )= a

compressTextInt:: String -> String
compressTextInt text = intercalate "." (map (\x -> show (unJust (elemIndex x alphabet))) text)

splitTextInt:: String -> [Int]
splitTextInt [] = []
splitTextInt text = [numberInt] ++ splitTextInt newText 
  where 
  number = takeWhile (\x -> x /= '.') text
  numberInt = read number::Int
  newText = drop (length number + 1) text

deCompressTextInt:: String -> String
deCompressTextInt text = map (\x -> alphabet !! x)(splitTextInt text)

compressTextAmountChars:: String -> String
compressTextAmountChars text = concat (map (\g -> [head g] ++ (show (length g))) (group text))

decompressTextAmountChars:: String -> String
decompressTextAmountChars text = concat (map (\(char, amount) -> take amount (cycle [char]) )(splitText text))

splitText:: String -> [(Char, Int)]
splitText [] = []
splitText (x:text) = [(char, amount)]  ++ splitText followingText
              where
              char = x
              followingDigits = takeWhile (\x -> elem x digits) text
              amount = read followingDigits::Int
              followingText = drop (length followingDigits) text

compressText2:: String -> Maybe BinaryTree ->  [Bit]
compressText2 text tree = compressText tree (compressTextInt text)

deCompressText2:: [Bit] -> Maybe BinaryTree -> String
deCompressText2 bits tree = deCompressTextInt (deCompressText tree bits)
