import Data.List

--DATA STRUCTURES --

-- Note: all indexing starts at 0.
--
-- Graph data type represents an undirected graph. 
-- mySize refers to the number of vertices within the graph.
-- myEdges is an adjacency list of edges for each vertex. The inner list is a list of vertices for that specific vertex (represented by the outer list) is connected to.
-- For instance a Graph : [[0],[1]] represents a graph of two nodes which are each connected to one another. (A line).

data Graph = Graph { mySize :: Int, myEdges :: [[Int]] } deriving Show

-- IN THIS FILE --
--
-- mergeMinkowskiSum :: Graph -> Graph -> Graph
-- minkowskiSum :: Graph -> Graph -> Graph
--getMyFourNeighbours :: Int -> Int -> Int -> Int -> [Int]
--getMyPosition :: Int -> Int -> Int -> Int -> Int
--recurseList :: Graph -> Graph
--
--genTorus :: Int -> Int -> Graph
--genHypercube :: Int -> Graph
--


-- AUXILIARY FUNCTIONS --

--merger function to take two disconnected identical graphs and joins them.
--    *         *                *----------*
--    |         |				 |          |
--    |         |        ==>>    |          |
--    |         |        ==>>    |          |
--    |         |                |          |
--    *         *                *----------*
-- It could be merged with the function below but Ive written it as two functions for neatness.

mergeMinkowskiSum :: Graph -> Graph -> Graph
mergeMinkowskiSum myFirstGraph mySecondGraph =
				Graph
				{
					mySize = tempSize,
					myEdges =
					[
					 	(myEdges ( minkowskiSum myFirstGraph mySecondGraph )) !! myCounter
						++ [
							((myCounter + (mySize myFirstGraph)) `mod` tempSize )
						   ]
						 | myCounter <- [0..(tempSize - 1)]
				    ]		
				}
				where
					tempSize = (mySize myFirstGraph) + (mySize mySecondGraph)

-- minkowskiSum used to clone its input and return two disconnected graphs (interpreted as one)
--
--      *								*         *
--      |								|         |
--      |                   ==>>        |         |
--      |                   ==>>        |         |
--      |								|         |
--      * 								*         *
--

minkowskiSum :: Graph -> Graph -> Graph
minkowskiSum myFirstGraph mySecondGraph =
			 Graph
			{
				mySize = (mySize myFirstGraph) + (mySize mySecondGraph),
				myEdges = (myEdges myFirstGraph) ++  
						  [[secondHalf + (mySize myFirstGraph) | secondHalf <- myCounter]
						  					| myCounter <- myEdges mySecondGraph]
			}

-- returns the four neighbours of a graph (neighbouring vertically and horizontally)
--              -----
--           0  | 1 |   2            
--          --------------
--          | 3 |(x,y)| 5 |       (1,1,width=3,height=3) => [1,3,5,7]
--          -------------- 
--            6  | 7 |  8
--               -----
--
--

getMyFourNeighbours :: Int -> Int -> Int -> Int -> [Int]
getMyFourNeighbours x y myWidth myHeight = 
					 nub (sort [
									getMyPosition (x - 1) y myWidth myHeight,
									getMyPosition (x + 1) y myWidth myHeight,
									getMyPosition x (y - 1) myWidth myHeight,
									getMyPosition x (y + 1) myWidth myHeight
								]) 

-- returns exact unique position in the graph with wrapping so that negative and larger numbers are excepted.
            
--           0  1  (2,0)      (2,0, 3, 3) =>  2        
--          
--           3 (1,1) 5        (1,1, 3, 3) => 1
--           
--          (0,2)  7  8       (2, 0, 3, 3) => 6
--               
getMyPosition :: Int -> Int -> Int -> Int -> Int
getMyPosition x y myWidth myHeight = 
	(x - ((x `div` myWidth) * myWidth))  +  (y - ((y `div` myHeight) * myHeight)) * myWidth
--
-- This simply returns the next graph down.. Its in place for a 'list recursion'
-- Graph{3, [[1],[2],[0]]} -> Graph{2, [1],[2]}
--

listRecurse :: Graph -> Graph
listRecurse g =  Graph{mySize = (mySize g) - 1, myEdges = init (myEdges g)}

-- IMPLEMENTATIONS --
--
-- genTorus iterates through a constructed graph generating edges on each selected vertex.
-- It takes dimensions x y for instance genTorus 3 creates a 3x3 Graph.
genTorus :: Int -> Int -> Graph
genTorus x y = if x <= 0 && y <= 0 then Graph{mySize = 0, myEdges = []}
				else Graph
				{
					mySize = x*y,
					myEdges = [getMyFourNeighbours xs ys x y | ys <- [0..y-1], xs <- [0..x-1]]
				}
--genHypercube takes a dimension (x) and recursively constructs a hypercube up to that dimension x.
-- it does this by recursing down to the base case, multiplying that graph and then merging the graphs together.
--It is explained above how this works.
genHypercube :: Int -> Graph
genHypercube x | x < 0 = Graph{mySize = 0, myEdges = [[]] }
			   | x == 0 = Graph {mySize = 1, myEdges = [[]]}
			   | x > 0 = Graph 
			  			{
			  				mySize = (mySize (minkowskiSum (genHypercube (x - 1)) (genHypercube (x - 1)))),
			  				myEdges = 
			  				  (myEdges (mergeMinkowskiSum (genHypercube(x - 1)) (genHypercube(x - 1))))	 		 
					    }
