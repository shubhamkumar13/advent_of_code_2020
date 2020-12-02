import qualified Data.ByteString.Char8 as C
import System.Environment

input2 = 
    map C.pack
    [ "1-3 a: abcde"
    , "1-3 b: cdefg"
    , "2-9 c: ccccccccc" ]

isValidFrequency :: Int -> Int -> C.ByteString -> C.ByteString -> Bool
isValidFrequency index1 index2 character string =
    xor (C.index string (index1-1) == C.head character) (C.index string (index2-1) == C.head character)
    where
        xor a b = (a && not b) || (not a && b)
    
f str =
    let [policy, character, string] = C.split ' ' str in
    let [start, end] = C.split '-' policy in
    let index1 = read (C.unpack start) :: Int in
    let index2 = read (C.unpack end) :: Int in
    isValidFrequency index1 index2 character string

solveProblem :: [String] -> String
solveProblem = show . length . filter (== True) . map (f . C.pack)

main :: IO ()
main = do
    args <- getArgs
    let f = solveProblem . lines
    input <- readFile (head args)
    putStr $ f input