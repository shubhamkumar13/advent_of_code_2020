import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let f = solveProblem . lines
    input <- readFile $ head args
    putStrLn $ f input

solveProblem :: [String] -> String
solveProblem xs = show $ f 1 2 (map cycle xs) 0 0 0

f :: Int -> Int -> [String] -> Int -> Int -> Int -> Int
f xStep yStep matrix x y treeCount =
    if (y + yStep) < length matrix then 
        let value = flip (!!) (x + xStep) $ matrix !! (y + yStep) in
        if value == '#' then
            f xStep yStep matrix (x + xStep) (y + yStep) (treeCount + 1)
        else
            f xStep yStep matrix (x + xStep) (y + yStep) treeCount
    else
        treeCount

input3 =
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