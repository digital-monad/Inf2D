--findIdx :: [Int] -> Int -> [[Int]]
--findIdx x node = [[idx,node] | (idx,val) <- zip [0..] x, val /= 0]

--findRow :: Int -> [Int] -> [Int]
--findRow node graph = take numNodes (drop (node*numNodes) graph)
--  where numNodes = 5

-- Function needs to go through the branch ([Int]) and apply findIdx . findRow to it
-- (take numNodes (drop (node*numNodes) graph))

--out :: [Int] -> [Int] -> [[Int]]
--out branch graph = [findIdx (findRow n graph)| n <- branch]

next :: Branch -> Graph -> [Branch]
--next branch graph = [x : branch | x <- expandNode (head branch) graph]
next branch graph = map (\x -> x : branch) (expandNode graph $ head branch)
  where expandNode graph node = [idx | (idx,val) <- zip [0..] (take numNodes $ drop (node*numNodes) graph), val /= 0]
checkArrival :: Int -> Int -> Bool
checkArrival dest curr = dest == curr

-- Breadth first search takes : GRAPH | DESTINATION NODE | NEXT FUNC | SEARCH AGENDA | VISITED NODES ||

breadthFirstSearch :: Graph -> Node -> (Branch -> Graph -> [Branch]) -> [Branch] -> [Node] -> Maybe Branch
breadthFirstSearch graph dest nex agenda visited
{-| ALGORITHM:
Check if HEAD of any branch is DEST - use checkArrival
If so then return that branch - Use filter with guards?
Otherwise recurse with:  {nex-expanded branches whose HEAD is not in VISITED} and {VISITED including all the HEADS of the new branches}
|-}
  | length foundNodes /= 0 = head newNodes
  | otherwise = breadthFirstSearch graph dest nex () ()
  where foundNodes = filter (\x -> checkArrival . dest $ head x) agenda
map nex (filter (\x -> not $ elem (head x) visited) agenda)
