module Lib where

import Data.List (group, intersperse, intercalate, elemIndex)
import Data.Char

import Tree
import HuffmanCode
import FrequencyAnalysis


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
