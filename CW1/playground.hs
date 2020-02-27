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

-- [0,1,1,0,0,0,0,0,1,0,0,1,0,1,0,0,0,0,0,1,0,0,0,0,0]

--next :: Branch -> Graph -> [Branch]
--next branch graph = foldl (++) [] (map (expandNode graph) branch)
--  where expandNode graph node = [[idx,node] | (idx,val) <- zip [0..] (take numNodes (drop (node*numNodes) graph)), val /= 0]
--        numNodes = 5 -- Not necessary in final implemention - numNodes is given

nexte :: Branch -> Graph -> [Branch]
--nexte branch graph = [x : branch | x <- expandNode (head branch) graph]
nexte branch graph = map (\x -> x : branch) (expandNode graph $ head branch)
  where expandNode graph node = [idx | (idx,val) <- zip [0..] (take numNodes $ drop (node*numNodes) graph), val /= 0]
        numNodes = 5

checkArrival :: Node -> Node -> Bool
checkArrival dest curr = dest == curr

-- Breadth first search takes : GRAPH | DESTINATION NODE | NEXT FUNC | SEARCH AGENDA | VISITED NODES ||

breadthFirstSearch :: Graph -> Node -> (Branch -> Graph -> [Branch]) -> [Branch] -> [Node] -> Branch
breadthFirstSearch graph dest nex agenda visited
{-| ALGORITHM:
Check if HEAD of any branch is DEST - use checkArrival
If so then return that branch - Use filter with guards?
Otherwise recurse with:  {nex-expanded branches whose HEAD is not in VISITED} and {VISITED including all the HEADS of the new branches}
|-}
  | length foundNodes /= 0 = head foundNodes
  | otherwise = breadthFirstSearch graph dest nex newAgenda newVisited
  where foundNodes = filter (\x -> checkArrival dest (head x)) agenda
        newAgenda = foldl (++) [] [nex b graph | b <- (filter (\x -> not $ elem (head x) visited) agenda)]
        newVisited = visited ++ [head x | x <- agenda]
