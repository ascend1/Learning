import System.IO
import Data.List as L

main = do
    tmpLine <- getLine
    appendFile "work.txt" (tmpLine ++ "\n")

shortLine :: String -> String
shortLine = unlines . filter (\line -> let len = length' line in (len < 10 && len > 1)) . lines

reverseWords = unwords . reverse . words

uniqueLength :: (Eq a) => [a] -> Int
uniqueLength = length' . nub

length' args = sum' [1 | _ <- args]

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

max' :: (Ord a) => [a] -> a
max' = foldl1 (\acc x -> if x > acc then x else acc)

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

take' :: (Num a, Ord a) => a -> [b] -> [b]
take' _ [] = []
take' n (x:xs)
	| n <= 0 = []
	| otherwise = x:take' (n - 1) xs

qsort :: (Ord a) => [a] -> [a]
qsort []  = []
qsort (x:xs) = 
	let 
		firstSorted = qsort [f | f <- xs, f < x]
		secondSorted = qsort [s | s <- xs, s >= x]
	in firstSorted ++ [x] ++ secondSorted

bmi :: (RealFloat a) => a -> a -> String
bmi h w
	| getbmi <= skinny = "Slim"
	| getbmi <= normal = "Normal"
	| getbmi <= fat = "Fat"
	| otherwise = "...Program is broken"
	where 
		getbmi = w / (h ^ 2)
		(skinny, normal, fat) = (18.5, 25.0, 30.0)
