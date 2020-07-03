unshuffle :: [a] -> [a]
unshuffle [] = []
unshuffle xs = take 1 (reverse xs) ++ take (length xs - 1) xs

compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs) = (if x == y then [] else [x]) ++ compress(y:xs)

someFunc :: [a] -> [a]
someFunc (x:y:xs) = [x,y]

merge ::Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
	| x <= y = x : merge xs (y:ys)
	| otherwise = y : merge (x:xs) ys

someOtherFunc :: [a] -> [a]
someOtherFunc [] = []
someOtherFunc [x] = [x]
someOtherFunc (x:xs) = x : someOtherFunc (xs)
--[2,3,4,4]

recurse ::Num a => [a] -> [a]
recurse [] = []
recurse [x] = [x]
recurse xs = recurse (tail xs) ++ [1,2,3,4]

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
