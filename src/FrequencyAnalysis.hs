module FrequencyAnalysis where

-- berechnet die absolute Häufigkeit für Buchstaben in einem Text
frequency:: [Char] -> [Char] -> [(Char,Int)]
frequency text alphabet = zip alphabet (map (\char -> countAmountEl text char) alphabet)

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
