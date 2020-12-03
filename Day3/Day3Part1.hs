import qualified Data.Text as T
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let f = solveProblem . map T.pack . lines
    input <- readFile $ head args
    putStrLn $ f input

solveProblem :: [T.Text] -> String
solveProblem xs = show $ f 0 0  (T.length $ head xs) (tail xs)

f :: Int -> Int -> Int -> [T.Text] -> Int
f indexCount treeCount len (x:xs) = let character = T.index x (mod (indexCount + 3) len) in
    if character == '#' then
        f (mod (indexCount + 3) len) (treeCount + 1) len xs
    else
        f (mod (indexCount + 3) len) treeCount len xs
f _ treeCount _ [] = treeCount

input3 =
    map T.pack
    [ "..##.........##.........##.........##.........##.........##......."
    , "#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#.."
    , ".#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#."
    , "..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#"
    , ".#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#."
    , "..#.##.......#.##.......#.##.......#.##.......#.##.......#.##....."
    , ".#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#"
    , ".#........#.#........#.#........#.#........#.#........#.#........#"
    , "#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#..."
    , "#...##....##...##....##...##....##...##....##...##....##...##....#"
    , ".#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#" ]