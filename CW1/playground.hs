import Data.List
type Node = Int
type Branch = [Int]
type Graph = [Int]
numNodes :: Int
numNodes = 5
-- [0,1,1,0,0,0,0,0,1,0,0,1,0,1,0,0,0,0,0,1,0,0,0,0,0]

nexte :: Branch -> Graph -> [Branch]
--nexte branch graph = [x : branch | x <- expandNode (head branch) graph]
nexte branch graph = map (\x -> x : branch) (expandNode graph $ head branch)
  where expandNode graph node = [idx | (idx,val) <- zip [0..] (take numNodes $ drop (node*numNodes) graph), val /= 0]

checkArrival :: Node -> Node -> Bool
checkArrival dest curr = dest == curr

breadthFirstSearch :: Graph -> Node -> (Branch -> Graph -> [Branch]) -> [Branch] -> [Node] -> Branch
breadthFirstSearch graph dest nex agenda visited
  | length foundNodes /= 0 = head foundNodes
  | otherwise = breadthFirstSearch graph dest nex newAgenda newVisited
  where foundNodes = filter (\x -> checkArrival dest (head x)) agenda
        newAgenda = foldl (++) [] [nex b graph | b <- (filter (\x -> not $ elem (head x) visited) agenda)]
        newVisited = visited ++ [head x | x <- agenda]

dfs :: Graph -> Node -> (Branch -> Graph -> [Branch]) -> [Branch] -> Int -> [Node] -> Branch
dfs graph dest nex (a:genda) limit visited
  | checkArrival dest $ head a = a -- First check whether node is found
  | length a == limit+1 = dfs graph dest nex genda limit (head a : visited)
  | otherwise = dfs graph dest nex ((nex a graph) ++ genda) limit (head a : visited)

getHr :: [Int] -> Node -> Int
getHr hrList n = hrList!!n

cost :: Graph -> Branch -> Int
cost graph branch = sum (map (\(na,nb) -> graph!!(na * numNodes + nb)) (zip (reverse branch) ((tail . reverse) branch)))

aStarSearch :: Graph -> Node -> (Branch -> Graph -> [Branch]) -> ([Int] -> Node -> Int) -> [Int] -> (Graph -> Branch -> Int) -> [Branch] -> [Node] -> Branch
aStarSearch graph dest nex hr hrList cot (a:genda) visited
  | checkArrival dest $ head a = a -- First check whether node is found
  | otherwise = aStarSearch graph dest nex hr hrList cot [snd nextNode] (head a : visited)
  where nextNode = head . sort $ zip (map (\x -> hr hrList (head x) + cost graph x) (nex a graph)) (nex a graph)
