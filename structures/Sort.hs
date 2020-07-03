unshuffle :: [a] -> [a]
unshuffle [] = []
unshuffle xs = take 1 (reverse xs) ++ take (length xs - 1) xs

merge ::Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
	| x <= y = x : merge xs (y:ys)
	| otherwise = y : merge (x:xs) ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort ys) (mergeSort zs)
				where (ys,zs) = halve xs 

firstHalf:: [a] -> [a]
firstHalf xs = take ((length xs) `div` 2) xs

secondHalf:: [a] -> [a]
secondHalf xs = take ((length xs) `div` 2) (reverse xs)

halve :: [a] -> ([a],[a])
halve xs = splitAt (length xs `div` 2) xs
