module Tree where

-- BinaryTree wenn Blatt dann Char(Buchstabe) und Float(Häufigkeit)
data BinaryTree = BinaryTree {leftSubTree::Maybe BinaryTree, rightSubTree:: Maybe BinaryTree} | Leaf Char Float deriving (Show, Eq)

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

