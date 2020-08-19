module FrequencyAnalysis where
import Data.Char 

-- berechnet die absolute Häufigkeit für Buchstaben in einem Text
frequency:: [Char] -> [Char] -> [(Char,Int)]
frequency text alphabet = zip alphabet (map (\char -> countAmountEl text char) alphabet)



-------------------- Die schnellere Häufigkeitsanalyse--------------------------------------
-- Das Array ist in der Form
-- [(A, 5), (B, 3), (C, 8), ... (Z, 4)]

-- erzeugt das Array  mit 0en 
frequency2:: [Char] -> [Char]-> [(Char, Int)]
frequency2 c alphabet = frequency2' c [(alphabet !! i,0)| i <- [0..(length alphabet)-1]]

-- Zählt die Häufigkeit von Buchstaben in einem Text 
frequency2':: [Char] -> [(Char,Int)] -> [(Char,Int)]
frequency2' [] array = array
frequency2' (c:text) array = let updated = array in frequency2' text (increaseChar array c)

-- Verändert den Wert am index in der Liste zu einem  neuen Wert
updateList:: [a] -> Int -> a -> [a]
updateList array index newElem = let splitted = splitAt (index) array in fst splitted ++ [newElem] ++ tail (snd splitted)

-- konvertiert ein Buchstabe zu dem entsprechenden Index
charToIndex:: Char -> Int
charToIndex c = ord c - 65

-- erhöht für einen bestimmten Buchstaben die Zahl im Array
increaseChar:: [(Char, Int)] -> Char -> [(Char,Int)]
increaseChar array c = newArray
            where
            index = charToIndex c
            oldElem = array !! index
            updateElem = (fst oldElem, (snd oldElem) + 1)
            newArray = updateList array index updateElem
--------------------------------------------------------------------------------------            



-- die relative Häufigkeit aus der absoluten Häufigkeit
relativeFrequency:: [(Char, Int)] -> [(Char, Float)]
relativeFrequency freq = let summ =sum ( map (\x -> snd x) freq) in map (\tuple -> ( fst tuple,  ((fromIntegral (snd tuple)) / (fromIntegral summ)) * 100 ) ) freq


--  zählt wie oft ein Element ein einer Liste vorkommt
countAmountEl:: [Char] -> Char -> Int
countAmountEl list element = length (filter (\x -> x == element) list)

digits:: String
digits = "0123456789"

getUniques:: String -> String
getUniques text = getUniques' text []

getUniques':: String -> String -> String
getUniques' [] uniques = uniques
getUniques' (x:text) uniques = if (elem x uniques) then getUniques' text uniques else getUniques' text ([x] ++ uniques)

getTextsAlphabet:: String -> String
getTextsAlphabet text = getUniques text

-- Das Alphabet Groß
alphabet:: String
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
