length' args = sum' [1 | _ <- args]

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

max' :: (Ord a) => [a] -> a
max' [] = error "list cannot be empty"
max' [x] = x
max' (x:xs) = if x > maxTail then x else maxTail where maxTail = max' xs

take' :: (Num a, Ord a) => a -> [b] -> [b]
take' _ [] = []
take' n (x:xs)
	| n <= 0 = []
	| otherwise = x:take' (n - 1) xs

bmi :: (RealFloat a) => a -> a -> String
bmi h w
	| getbmi <= skinny = "Slim"
	| getbmi <= normal = "Normal"
	| getbmi <= fat = "Fat"
	| otherwise = "...Program is broken"
	where 
		getbmi = w / (h ^ 2)
		(skinny, normal, fat) = (18.5, 25.0, 30.0)