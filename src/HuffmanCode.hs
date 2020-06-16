module HuffmanCode where

import Tree
import FrequencyAnalysis


-- Bit 0 oder 1
data Bit = Bit {v::Int}  deriving (Show, Eq) -- 0 1

-- gibt Werte der Bits zurück
getValuesBits:: [Bit] -> [Int]
getValuesBits bs = map v bs

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