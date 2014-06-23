collatz_len :: Int -> Int -> Int
collatz_len acc 1 = acc
collatz_len acc x = collatz_len (acc+1) y
	where y = if even x then (quot x 2) else (3*x +1)

max_collatz' n i acc = if i > n then acc else max_collatz' n (i+1) (!max acc (collatz_len 1 i))
max_collatz n = max_collatz' n 1 0
