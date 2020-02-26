--findIdx :: [Int] -> Int -> [[Int]]
--findIdx x node = [[idx,node] | (idx,val) <- zip [0..] x, val /= 0]

--findRow :: Int -> [Int] -> [Int]
--findRow node graph = take numNodes (drop (node*numNodes) graph)
--  where numNodes = 5

-- Function needs to go through the branch ([Int]) and apply findIdx . findRow to it
-- (take numNodes (drop (node*numNodes) graph))

--out :: [Int] -> [Int] -> [[Int]]
--out branch graph = [findIdx (findRow n graph)| n <- branch]

next :: [Int] -> [Int] -> [[Int]]
next branch graph = foldl (++) [] (map (expandNode graph) branch)
  where expandNode graph node = [[idx,node] | (idx,val) <- zip [0..] (take numNodes (drop (node*numNodes) graph)), val /= 0]
        numNodes = 5

checkArrival :: Int -> Int -> Bool
checkArrival dest curr = dest == curr
