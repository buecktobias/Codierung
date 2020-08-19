module FrequencyAnalysis where

-- berechnet die absolute Häufigkeit für Buchstaben in einem Text
frequency:: [Char] -> [Char] -> [(Char,Int)]
frequency text alphabet = zip alphabet (map (\char -> countAmountEl text char) alphabet)

frequency2:: [Char] -> [Char]-> [(Char, Int)]
frequency2 c alphabet = [(alphabet !! i,0)| i <- [0..(length alphabet)-1]]


updateList:: [a] -> Int -> a -> [a]
updateList array index newElem = let splitted = splitAt (index) array in fst splitted ++ [newElem] ++ tail (snd splitted)


increaseChar:: [(Char, Int)] -> Int -> [(Char,Int)]
increaseChar array index = newArray
            where
            oldElem = array !! index
            updateElem = (fst oldElem, (snd oldElem) + 1)
            newArray = updateList array index updateElem
            
frequency2':: [Char] -> [(Char,Int)] -> [(Char,Int)]
frequency2' [] array = array
frequency2' (c:text) array = let updated = array in array

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
