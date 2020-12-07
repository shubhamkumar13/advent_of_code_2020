import System.Environment
import Data.List ( intersect )
import qualified Data.Text as T

main :: IO ()
main = do
    args <- getArgs
    input <- readFile $ head args
    let inputList = day6Fmt input
    putStrLn $ show $ f inputList

day6Fmt :: String -> [[String]]
day6Fmt input = (map lines . map T.unpack . T.splitOn (T.pack "\n\n") . T.pack) input

f :: [[String]] -> Int
f =  sum . map (length . foldr1 intersect)

input6 =
    "abc\n\n\
    \a\n\
    \b\n\
    \c\n\n\
    \ab\n\
    \ac\n\n\
    \a\n\
    \a\n\
    \a\n\
    \a\n\n\
    \b"