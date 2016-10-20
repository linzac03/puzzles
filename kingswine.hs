import Data.List

num :: Int
num = 1000
bottles :: [Int]
bottles = map (\x -> 0) [1..num]
servants :: [(Int,Int)]
servants = map (\x -> (0,0)) [1..10]

mark n (x:xs) chk
	| chk < n		= x : mark n xs (chk+1)
	| chk == n  = 1 : mark n xs (chk+1) 
	| chk > n		= x : mark n xs (chk+1)

mark _ [] _ = []

ngen :: Int -> Int -> [Int]
ngen 0 _ = []
ngen n m = m : ngen (n-1) (m+1)

genlist :: Int -> [[Int]]
genlist n = tail $ subsequences $ ngen n 1

taste :: [Int] -> [(Int,Int)] -> [[Int]] -> [(Int,Int)]
taste [] srvnts _ 					 = srvnts
taste (b:bs) srvnts (xs:xss) = taste bs (tasten srvnts b xs) xss
 
tasten :: [(Int,Int)] -> Int -> [Int] -> [(Int, Int)]
tasten srvnts bttl [] = srvnts
tasten srvnts bttl (x:xs) = tasten (tasten' srvnts bttl x 1) bttl xs 
	where
		tasten' :: [(Int,Int)] -> Int -> Int -> Int -> [(Int,Int)]
		tasten' [] btl taster pos = []
		tasten' (srv:srvs) btl taster pos
			| pos == taster && btl == 1		= (1,(snd srv + 1)) : tasten' srvs btl taster (pos+1) 
			| otherwise										= srv : tasten' srvs btl taster (pos+1)

		
test n = reverse $ taste (mark n bottles 1) servants (genlist 10)



