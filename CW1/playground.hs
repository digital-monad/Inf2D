import Data.List
--findIdx :: [Int] -> Int -> [[Int]]
--findIdx x node = [[idx,node] | (idx,val) <- zip [0..] x, val /= 0]

--findRow :: Int -> [Int] -> [Int]
--findRow node graph = take numNodes (drop (node*numNodes) graph)
--  where numNodes = 5

-- Function needs to go through the branch ([Int]) and apply findIdx . findRow to it
-- (take numNodes (drop (node*numNodes) graph))

--out :: [Int] -> [Int] -> [[Int]]
--out branch graph = [findIdx (findRow n graph)| n <- branch]
type Node = Int
type Branch = [Int]
type Graph = [Int]
numNodes :: Int
numNodes = 5
-- [0,1,1,0,0,0,0,0,1,0,0,1,0,1,0,0,0,0,0,1,0,0,0,0,0]

--next :: Branch -> Graph -> [Branch]
--next branch graph = foldl (++) [] (map (expandNode graph) branch)
--  where expandNode graph node = [[idx,node] | (idx,val) <- zip [0..] (take numNodes (drop (node*numNodes) graph)), val /= 0]
--        numNodes = 5 -- Not necessary in final implemention - numNodes is given

nexte :: Branch -> Graph -> [Branch]
--nexte branch graph = [x : branch | x <- expandNode (head branch) graph]
nexte branch graph = map (\x -> x : branch) (expandNode graph $ head branch)
  where expandNode graph node = [idx | (idx,val) <- zip [0..] (take numNodes $ drop (node*numNodes) graph), val /= 0]

checkArrival :: Node -> Node -> Bool
checkArrival dest curr = dest == curr

-- Breadth first search takes : GRAPH | DESTINATION NODE | NEXT FUNC | SEARCH AGENDA | VISITED NODES ||

breadthFirstSearch :: Graph -> Node -> (Branch -> Graph -> [Branch]) -> [Branch] -> [Node] -> Maybe Branch
breadthFirstSearch graph dest nex agenda visited
{-| ALGORITHM:
Check if HEAD of any branch is DEST - use checkArrival
If so then return that branch - Use filter with guards?
Otherwise recurse with:  {nex-expanded branches whose HEAD is not in VISITED} and {VISITED including all the HEADS of the new branches}
|-}
  | null agenda = Nothing
  | length foundNodes /= 0 = Just (head foundNodes)
  | otherwise = breadthFirstSearch graph dest nex newAgenda newVisited
  where foundNodes = filter (\x -> checkArrival dest (head x)) agenda
        newAgenda = filter (not . null) (foldl (++) [] [nex b graph | b <- (filter (\x -> not $ elem (head x) visited) agenda)])
        newVisited = visited ++ [head x | x <- agenda]

dfs :: Graph -> Node -> (Branch -> Graph -> [Branch]) -> [Branch] -> Int -> [Node] -> Maybe Branch
dfs _ _ _ [] _ _ = Nothing
dfs graph dest nex (a:genda) limit visited
  | checkArrival dest $ head a = Just a -- First check whether node is found
  | length a == limit+1 = dfs graph dest nex genda limit (head a : visited)
  | otherwise = dfs graph dest nex ((nex a graph) ++ genda) limit (head a : visited)

getHr :: [Int] -> Node -> Int
getHr hrList n = hrList!!n

cost :: Graph -> Branch -> Int
cost graph branch = sum (map (\(na,nb) -> graph!!(na * numNodes + nb)) (zip (reverse branch) ((tail . reverse) branch)))

aStarSearch :: Graph -> Node -> (Branch -> Graph -> [Branch]) -> ([Int] -> Node -> Int) -> [Int] -> (Graph -> Branch -> Int) -> [Branch] -> [Node] -> Maybe Branch
aStarSearch _ _ _ _ _ _ [[]] _ = Nothing
aStarSearch graph dest nex hr hrList cot (a:genda) visited
  | checkArrival dest $ head a = Just a -- First check whether node is found
  | otherwise = aStarSearch graph dest nex hr hrList cot [snd nextNode] (head a : visited)
  where nextNode
          | null (nex a graph) = (0,[])
          | otherwise = head . sort $ zip (map (\x -> hr hrList (head x) + cot graph x) (nex a graph)) (nex a graph)
