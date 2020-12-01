import Data.Maybe

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let f = solveProblem . lines
  input1 <- readFile (head args)
  putStr (f input1)

solveProblem :: [String] -> String
solveProblem xs = show $ solveProblem' $ map (\x -> read x :: Integer) xs

solveProblem' :: [Integer] -> Integer
solveProblem' (x:xs) =
	let result = foldr (\x acc -> if x + acc == 2020 then x * acc else acc) x xs in
	if result == x then solveProblem' xs else result
