import System.Environment
import qualified Data.Text as T
import qualified Data.Set as S

main :: IO ()
main = do
    args <- getArgs
    input <- readFile $ head args
    let inputList = day6Fmt input
    putStrLn $ f inputList

day6Fmt :: String -> [[T.Text]]
day6Fmt input = map (T.split ( == '\n')) $ T.splitOn (T.pack "\n\n") (T.pack input)

f :: [[T.Text]] -> String
f =  show . sum . map (S.size . S.fromList . T.chunksOf 1 . T.concat)

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