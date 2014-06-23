max_path' x [] = x
max_path' x xs = max_path' (zipWith max (zipWith (+) (init x) (head xs)) (zipWith (+) (tail x) (head xs))) (tail xs)

max_path xs = max_path' (head xs) (tail xs)

main = do
	f <- readFile "problem_18.txt"
	let g = fmap words $ lines f
	let h = (map.map) read g
	print $ max_path $ reverse h	
