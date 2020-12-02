import Data.Maybe
import System.Environment

main :: IO ()
main = do
	args <- getArgs
	let f = solveProblem . lines
	input1 <- readFile $ head args
	putStr $ f input1

solveProblem :: [String] -> String
solveProblem xs = solveProblem' $ mkThruple xs

solveProblem' :: [(Integer, Integer, Integer)] -> String
solveProblem' xs = show $ foldr (\(a, b, c) acc -> if a + b + c == 2020 then a * b * c else acc) 0 xs

mkTuple :: Eq b => [b] -> [(b,b)]
mkTuple xs = concatMap (catMaybes . (\a -> map (\aa -> if aa == a then Nothing else Just (a, aa)) xs)) xs

mkThruple :: [String] -> [(Integer, Integer, Integer)]
mkThruple xs = catMaybes $ concatMap (\x -> map (\(a,b) -> if x == a || x == b then Nothing else Just (read x :: Integer, read a :: Integer, read b :: Integer)) (mkTuple xs)) xs
