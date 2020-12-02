import qualified Data.ByteString.Char8 as C
import System.Environment

input2 = 
    map C.pack
    [ "1-3 a: abcde"
    , "1-3 b: cdefg"
    , "2-9 c: ccccccccc" ]

isValidFrequency :: Int -> Int -> C.ByteString -> C.ByteString -> Bool
isValidFrequency start end character string =
        let counter = C.count (C.head character) string in
        counter >= start && counter <= end 

f str =
    let [policy, character, string] = C.split ' ' str in
    let [start, end] = C.split '-' policy in
    let starty = read (C.unpack start) :: Int in
    let endy = read (C.unpack end) :: Int in
    isValidFrequency starty endy character string

solveProblem :: [String] -> String
solveProblem = show . length . filter (== True) . map (f . C.pack)

main :: IO ()
main = do
    args <- getArgs
    let f = solveProblem . lines
    input <- readFile (head args)
    putStr $ f input