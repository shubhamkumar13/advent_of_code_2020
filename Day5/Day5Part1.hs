import qualified Data.Text as T
import System.Environment

rowSize = 128
colSize = 8

input5 =
    map T.pack
    [ "FBFBBFFRLR"
    , "BFFFBBFRRR"
    , "FFFBBBFRRR"
    , "BBFFBBFRLL"
    ]

main :: IO ()
main = do
    args <- getArgs
    input <- readFile $ head args
    putStrLn $ (show . foldr max 0 . map output . day5Fmt) input

day5Fmt :: String -> [T.Text]
day5Fmt = map T.pack . lines

separateInstr :: T.Text -> (T.Text, T.Text)
separateInstr xs = T.splitAt (T.length xs - 3) xs

rowDecision :: T.Text -> (Integer, Integer) -> Integer
rowDecision xs (lower, upper) = 
    if T.null xs then min lower upper
    else
        case T.head xs of
            'F' -> rowDecision (T.tail xs) (lower, div (lower + upper) 2)
            'B' -> rowDecision (T.tail xs) (div (lower + upper) 2 + 1, upper)
            _   -> min lower upper

colDecision :: T.Text -> (Integer, Integer) -> Integer
colDecision xs (lower, upper) = 
    if T.null xs then max lower upper
    else
        case T.head xs of
            'L' -> colDecision (T.tail xs) (lower, div (lower + upper) 2)
            'R' -> colDecision (T.tail xs) (div (lower + upper) 2 + 1, upper)
            _   -> max lower upper

-- somefunc ch (lower, upper) =
--     case ch of
--         'L' -> (lower, (div (lower + upper) 2))
--         'R' -> ((div (lower + upper) 2) + 1, upper)

output xs = (row * 8) + col
    where
        row = rowDecision (fst $ separateInstr xs) (0, rowSize - 1)
        col = colDecision (snd $ separateInstr xs) (0, colSize - 1)