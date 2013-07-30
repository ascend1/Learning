import Data.List

solveRPN :: String -> Float
solveRPN = head . foldl checkElement [] . words
    where checkElement (x:y:ys) "+" = (y + x):ys
          checkElement (x:y:ys) "-" = (y - x):ys
          checkElement (x:y:ys) "*" = (y * x):ys
          checkElement (x:y:ys) "/" = (y / x):ys
          checkElement (x:y:ys) "^" = (y ** x):ys
          checkElement xs numberString = (read numberString):xs
