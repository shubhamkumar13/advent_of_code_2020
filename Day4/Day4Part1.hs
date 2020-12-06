import System.Environment
import qualified Data.Text as T

main :: IO ()
main = do
    args <- getArgs
    input <- readFile $ head args
    putStrLn $ (part1h . part1g . part1f) input

input4 =
    "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\
    \byr:1937 iyr:2017 cid:147 hgt:183cm\n\n\
    \iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n\
    \hcl:#cfa07d byr:1929\n\n\
    \hcl:#ae17e1 iyr:2013\n\
    \eyr:2024\n\
    \ecl:brn pid:760753108 byr:1931\n\
    \hgt:179cm\n\n\
    \hcl:#cfa07d eyr:2025 pid:166559648\n\
    \iyr:2011 ecl:brn hgt:59in\n"

p :: [[T.Text]] -> [[(T.Text, T.Text)]]
p = map 
    (map (\y -> 
        let someVal = T.splitOn (T.pack ":") y in
        (head someVal, (head . tail) someVal)))

part1f :: String -> [[T.Text]]
part1f str =
    map (T.split (\x -> x == '\n' || x == ' '))
    $ T.splitOn (T.pack "\n\n") (T.pack str)

part1Fmt = map (filter (== T.pack "") . map fst) . p 

part1g :: [[T.Text]] -> [Bool]
part1g = map (\x -> length x == 8 ||  (T.pack "cid" `notElem` x) && everythingElse x) . part1Fmt
    where
        everythingElse xs = foldr (flip (&&) . (`elem` xs)) True theList

part1h :: [Bool] -> String
part1h = show . length . filter id

is4digit = (== 0) . flip div 10000
isbyrValid n = is4digit n && (n >= 1920 && n <= 2002)
isiyrValid n = is4digit n && (n >= 2010 && n <= 2020)
iseyrValid n = is4digit n && (n >= 2020 && n <= 2030)
ishgtValid s | T.isSuffixOf (T.pack "cm") s = 
                    let v = (\x -> read x :: Integer) $ T.unpack . head $ T.splitOn (T.pack "cm") s in
                    v >= 150 && v <= 193
             | T.isSuffixOf (T.pack "in") s =
                    let v = (\x -> read x :: Integer) $ T.unpack . head $ T.splitOn (T.pack "in") s in
                    v >= 59 && v <= 76
-- ishclValid n = let v = T.splitOn (T.pack "#") n in
--     if length v == 2 then


theList = 
    map T.pack
    [ "byr"
    , "iyr"
    , "eyr"
    , "hgt"
    , "hcl"
    , "ecl"
    , "pid"]