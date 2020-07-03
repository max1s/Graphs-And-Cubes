data Tree = Leaf Int | Node Tree Int Tree

slice :: [a] -> Int -> Int -> [a]
slice (x:xs) i k
		| i > 1 = slice xs (i-1) (k-1)
		| k < 1 = []
		| otherwise = x : slice xs (i-1) (k-1)

dup :: [a] -> [a]
dup [] = []
dup (x:xs) = [x,x] ++ dup xs


iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

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
				       
nodes :: Tree -> Int
nodes (Leaf x) = 1
nodes (Node l _ r) = 1 + nodes l + nodes r

isComplete :: Tree -> Bool
isComplete (Leaf x) = True
isComplete (Node l _ r) = (nodes l) == (nodes r)
  