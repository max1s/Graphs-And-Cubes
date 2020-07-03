import Data.List

--DATA STRUCTURES --

-- Note: all indexing starts at 0.
--
-- Graph data type represents an undirected graph. 
-- mySize refers to the number of vertices within the graph.
-- myEdges is an adjacency list of edges for each vertex. The inner list is a list of vertices for that specific vertex (represented by the outer list) is connected to.
-- For instance a Graph : [[0],[1]] represents a graph of two nodes which are each connected to one another. (A line).

data Graph = Graph { mySize :: Int, myEdges :: [[Int]] } deriving Show

--
greedyColouring :: Graph -> ([Int], Int)
greedyColouring Graph {mySize = 0 , myEdges = [] } = ([], 0)
greedyColouring g = ((fst myRecurse) ++ [myDeterminedColour], max (myDeterminedColour + 1) (snd myRecurse))
	where 
		myRecurse = greedyColouring (listRecurse g)
		myDeterminedColour = head ([0..(snd myRecurse)] \\ [(fst myRecurse) !! forbiddenColour 
			| forbiddenColour <- last (myEdges g), forbiddenColour < (length (fst myRecurse))])
