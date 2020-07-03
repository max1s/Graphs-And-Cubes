slice :: [a] -> Int -> Int -> [a]
slice (x:xs) i k
		| i > 1 = slice xs (i-1) (k-1)
		| k < 1 = []
		| otherwise = x : slice xs (i-1) (k-1)

ins :: Int -> [Int] -> [Int]
ins x [] = [x]
ins x (y:ys)
	| x <= y = x:y:ys
	| otherwise = y:ins x ys

minList ::  [Int] -> Int
minList xs = head ( iSort xs )

maxList ::  [Int] -> Int
maxList xs = last ( iSort xs )

altMinList :: [Int] -> Int
altMinList (x:xs) = currentMinList x xs

currentMinList :: Int -> [Int] -> Int
currentMinList x [] = x
currentMinList x (y:ys) 
			| x <= y = currentMinList x ys
			| otherwise = currentMinList y ys