-- Inf2d Assignment 1 2019-2020
-- Matriculation number:
-- {-# OPTIONS -Wall #-}


module Inf2d1 where

import Data.List (sort, elemIndices, elemIndex)
import ConnectFourWithTwist




{- NOTES:

-- DO NOT CHANGE THE NAMES OR TYPE DEFINITIONS OF THE FUNCTIONS!
You can write new auxillary functions, but don't change the names or type definitions
of the functions which you are asked to implement.

-- Comment your code.

-- You should submit this file when you have finished the assignment.

-- The deadline is the  10th March 2020 at 3pm.

-- See the assignment sheet and document files for more information on the predefined game functions.

-- See the README for description of a user interface to test your code.

-- See www.haskell.org for haskell revision.

-- Useful haskell topics, which you should revise:
-- Recursion
-- The Maybe monad
-- Higher-order functions
-- List processing functions: map, fold, filter, sortBy ...

-- See Russell and Norvig Chapters 3 for search algorithms,
-- and Chapter 5 for game search algorithms.

-}

-- Section 1: Uniform Search



-- The Node type defines the position of the agent on the graph.
-- The Branch type synonym defines the branch of search through the graph.
type Node = Int
type Branch = [Node]
type Graph= [Node]




-- 


-- The next function should return all the possible continuations of input search branch through the graph.
-- Your function should return an empty list if the input search branch is empty.
-- This implementation of next function does not backtrace branches.
next::Branch -> Graph ->  [Branch]
next branch graph = map (\x -> x : branch) (expandNode graph $ head branch)
  where expandNode graph node = [idx | (idx,val) <- zip [0..] (take (ceiling . sqrt . fromIntegral . length $ graph) $ drop (node*(ceiling . sqrt . fromIntegral . length $ graph)) graph), val /= 0]
       



-- |The checkArrival function should return true if the current location of the robot is the destination, and false otherwise.
checkArrival::Node -> Node -> Bool
checkArrival destination curNode = destination == curNode


explored::Node-> [Node] ->Bool
explored point exploredList = elem point exploredList

-- Section 3 Uniformed Search
-- | Breadth-First Search
-- The breadthFirstSearch function should use the next function to expand a node,
-- and the checkArrival function to check whether a node is a destination position.
-- The function should search nodes using a breadth first search order.

breadthFirstSearch::Graph -> Node->(Branch ->Graph -> [Branch])->[Branch]->[Node]->Maybe Branch
breadthFirstSearch graph dest nex agenda visited
  | null agenda = Nothing
  | length foundNodes /= 0 = Just (head foundNodes)
  | otherwise = breadthFirstSearch graph dest nex newAgenda newVisited
  where foundNodes = filter (\x -> checkArrival dest (head x)) agenda
        newAgenda = filter (not . null) (foldl (++) [] [nex b graph | b <- (filter (\x -> not $ elem (head x) visited) agenda)])
        newVisited = visited ++ [head x | x <- agenda]

   

-- | Depth-Limited Search
-- The depthLimitedSearch function is similiar to the depthFirstSearch function,
-- except its search is limited to a pre-determined depth, d, in the search tree.
depthLimitedSearch::Graph ->Node->(Branch ->Graph-> [Branch])->[Branch]-> Int->[Node]-> Maybe Branch
depthLimitedSearch _ _ _ [] _ _ = Nothing
depthLimitedSearch graph dest nex (a:genda) limit visited
  | checkArrival dest $ head a = Just a -- First check whether node is found
  | length a == limit+1 = depthLimitedSearch graph dest nex genda limit []
  | otherwise = depthLimitedSearch graph dest nex newAgenda limit (head a : visited)
  where newAgenda = filter (\x -> not (elem (head x) visited)) ((nex a graph)) ++ genda

-- [0,1,1,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,0,0,0,1, 0,0,0,0,0] 0 4 3

-- | Section 4: Informed search


-- | AStar Helper Functions

-- | The cost function calculates the current cost of a trace. The cost for a single transition is given in the adjacency matrix.
-- The cost of a whole trace is the sum of all relevant transition costs.
cost :: Graph ->Branch  -> Int
cost graph branch = sum (map (\(na,nb) -> graph!!(na * (ceiling . sqrt . fromIntegral . length $ graph) + nb)) (zip (reverse branch) ((tail . reverse) branch)))

    
-- | The getHr function reads the heuristic for a node from a given heuristic table.
-- The heuristic table gives the heuristic (in this case straight line distance) and has one entry per node. It is ordered by node (e.g. the heuristic for node 0 can be found at index 0 ..)  
getHr:: [Int]->Node->Int
getHr hrTable node = hrTable!!node  


-- | A* Search
-- The aStarSearch function uses the checkArrival function to check whether a node is a destination position,
---- and a combination of the cost and heuristic functions to determine the order in which nodes are searched.
---- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.

{--aStarSearch::Graph->Node->(Branch->Graph -> [Branch])->([Int]->Node->Int)->[Int]->(Graph->Branch->Int)->[Branch]-> [Node]-> Maybe Branch
aStarSearch _ _ _ _ _ _ [[]] _ = Nothing
aStarSearch graph dest nex hr hrList cot (a:genda) visited
  | checkArrival dest $ head a = Just a -- First check whether node is found
  | otherwise = aStarSearch graph dest nex hr hrList cot [snd nextNode] (head a : visited)
  where nextNode
          | null (nex a graph) = (0,[])
          | otherwise = head . sort $ zip (map (\x -> hr hrList (head x) + cot graph x) (nex a graph)) (nex a graph)

--}
aStarSearch::Graph->Node->(Branch->Graph -> [Branch])->([Int]->Node->Int)->[Int]->(Graph->Branch->Int)->[Branch]-> [Node]-> Maybe Branch
aStarSearch _ _ _ _ _ _ [] _ = Nothing
aStarSearch graph dest nex hr hrList cot (a:genda) visited
  | checkArrival dest $ head a = Just a
  | otherwise = aStarSearch graph dest nex hr hrList cot newAgenda (head newlyExpanded : visited)
  where newAgenda = filter (\x -> not (elem (head x) visited)) ((nex newlyExpanded graph) ++ (a:genda))
        newlyExpanded = snd (head (sort $ zip (map (\x -> hr hrList (head x) + cot graph x) (a:genda)) (a:genda)))
  

{--aStarSearch graph dest nex hr hrList cot agenda visited
  | null $ filter (\x -> not $ checkArrival dest (head x)) agenda = snd pickBranch -- All branches have arrived at the destination
  | otherwise = aStarSearch graph dest nex hr hrList cot newAgenda (visited) -- Continue mapping
  where pickBranch
        | null agenda = (0,[])
        | otherwise = head . sort $ zip (map (\x -> hr hrList (head x) + cot graph x) agenda) agenda
        newAgenda = foldl (++) [] [if checkArrival dest (head b) then [b] else next b graph | b <- agenda, not (elem (head b) visited)]
--}

-- | Section 5: Games
-- See ConnectFourWithTwist.hs for more detail on  functions that might be helpful for your implementation. 

--filter (\x -> not (elem (head x) visited)) (nex (snd (head (sort $ zip (map (\x -> hr hrList (head x) + cot graph x) (a:genda)))) graph) : (a:genda))

-- | Section 5.1 Connect Four with a Twist

 

-- The function determines the score of a terminal state, assigning it a value of +1, -1 or 0:
eval :: Game -> Int
eval board
  | checkWin board compPlayer = -1
  | checkWin board humanPlayer = 1
  | otherwise = 0

-- | The alphabeta function should return the minimax value using alphabeta pruning.
-- The eval function should be used to get the value of a terminal state. 
alphabeta:: Role -> Game -> Int
alphabeta  player game = alphabeta' player game (-2) 2

-- | OPTIONAL!
-- You can try implementing this as a test for yourself or if you find alphabeta pruning too hard.
-- If you implement minimax instead of alphabeta, the maximum points you can get is 10% instead of 15%.
-- Note, we will only grade this function IF YOUR ALPHABETA FUNCTION IS EMPTY.
-- The minimax function should return the minimax value of the state (without alphabeta pruning).
-- The eval function should be used to get the value of a terminal state.
minimax:: Role -> Game -> Int
minimax player board
  | terminal board = eval board
  | player == humanPlayer = maximum (map (minimax compPlayer) (movesAndTurns board humanPlayer))
  | player == compPlayer = minimum (map (minimax humanPlayer) (movesAndTurns board compPlayer))
{- Auxiliary Functions
-- Include any auxiliary functions you need for your algorithms below.
-- For each function, state its purpose and comment adequately.
-- Functions which increase the complexity of the algorithm will not get additional scores
-}
alphabeta' :: Role -> Game -> Int -> Int -> Int
alphabeta' player board alpha beta
  | terminal board = eval board
  | player == compPlayer = minimum (phiMin (movesAndTurns board compPlayer) alpha beta)
  | player == humanPlayer = maximum (phiMax (movesAndTurns board humanPlayer) alpha beta)

phiMax :: [Game] -> Int -> Int -> [Int] -- Returns a list of game evaluations
phiMax [] _ _ = []
phiMax (c:hildren) alpha beta
  | beta > alpha = nodeVal : phiMax hildren (max alpha nodeVal) beta
  | otherwise = []
  where nodeVal = alphabeta' compPlayer c alpha beta

phiMin :: [Game] -> Int -> Int -> [Int]
phiMin [] _ _ = []
phiMin (c:hildren) alpha beta
  | beta > alpha = nodeVal : phiMin hildren alpha (min beta nodeVal)
  | otherwise = []
  where nodeVal = alphabeta' humanPlayer c alpha beta
