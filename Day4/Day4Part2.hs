import System.Environment
import qualified Data.Text as T

main :: IO ()
main = do
    args <- getArgs
    input <- readFile $ head args
    putStrLn $ (part2h . part2p . part2g . part2f) input

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
    \iyr:2011 ecl:brn hgt:59in"

part2g :: [[T.Text]] -> [[(T.Text, T.Text)]]
part2g = 
    map 
    (map (\y -> 
        let someVal = T.splitOn (T.pack ":") y in
        (head someVal, (head . tail) someVal)))

part2f :: String -> [[T.Text]]
part2f str =
    map (T.split (\x -> x == '\n' || x == ' '))
    $ T.splitOn (T.pack "\n\n") (T.pack str)

part2h :: [Bool] -> String
part2h = show . length . filter id

part2p :: [[(T.Text, T.Text)]] -> [Bool]
part2p xs =
    map (\x -> (doesItContainAll x || doesItContainImp x) && (foldr (\y acc -> acc && (isAllValid y)) True x)) xs

doesItContainAll :: [(T.Text, T.Text)] -> Bool
doesItContainAll = foldr (\x acc -> acc && (flip elem theList $ fst x)) True 

doesItContainImp :: [(T.Text, T.Text)] -> Bool
doesItContainImp = foldr (\x acc -> acc && (flip elem theImpList $ fst x)) True 

isAllValid :: (T.Text, T.Text) -> Bool
isAllValid (a,b) | a == (T.pack "byr") = isbyrValid b
                 | a == (T.pack "iyr") = isiyrValid b
                 | a == (T.pack "eyr") = iseyrValid b
                 | a == (T.pack "hgt") = ishgtValid b
                 | a == (T.pack "hcl") = ishclValid b
                 | a == (T.pack "ecl") = iseclValid b
                 | a == (T.pack "pid") = ispidValid b
                 | a == (T.pack "cid") = True
                 | otherwise           = True

textToNum num = read (T.unpack num) :: Integer
is4digit n = (== 0) $ div (textToNum n) 10000 
isbyrValid n = is4digit n && (num >= 1920 && num <= 2002)
    where
        num = textToNum n
isiyrValid n = is4digit n && (num >= 2010 && num <= 2020)
    where
        num = textToNum n
iseyrValid n = is4digit n && (num >= 2020 && num <= 2030)
    where
        num = textToNum n
ishgtValid s | T.isSuffixOf (T.pack "cm") s = 
                    let v = (\x -> read x :: Integer) $ T.unpack . head $ T.splitOn (T.pack "cm") s in
                    v >= 150 && v <= 193
             | T.isSuffixOf (T.pack "in") s =
                    let v = (\x -> read x :: Integer) $ T.unpack . head $ T.splitOn (T.pack "in") s in
                    v >= 59 && v <= 76
             | otherwise = False
ishclValid n =
    let isHash = T.head n == '#' in
    let isAlphaNumericValid = (T.length (T.tail n) == 6) && T.all (\ch -> elem ch hexList) (T.tail n) in
    isHash && isAlphaNumericValid
iseclValid = flip elem (map T.pack ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
ispidValid n = T.length n == 9

theList = 
    map T.pack
    [ "byr"
    , "iyr"
    , "eyr"
    , "hgt"
    , "hcl"
    , "ecl"
    , "pid"
    , "cid"]

theImpList = 
    map T.pack
    [ "byr"
    , "iyr"
    , "eyr"
    , "hgt"
    , "hcl"
    , "ecl"
    , "pid"]

hexList =
    [ '0'
    , '1'
    , '2'
    , '3'
    , '4'
    , '5'
    , '6'
    , '7'
    , '8'
    , '9'
    , 'a'
    , 'b'
    , 'c'
    , 'd'
    , 'e'
    , 'f']

invalidPass =
    "eyr:1972 cid:100\n\
    \hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926\n\n\
    \iyr:2019\n\
    \hcl:#602927 eyr:1967 hgt:170cm\n\
    \ecl:grn pid:012533040 byr:1946\n\n\
    \hcl:dab227 iyr:2012\n\
    \ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277\n\n\
    \hgt:59cm ecl:zzz\n\
    \eyr:2038 hcl:74454a iyr:2023\n\
    \pid:3556412378 byr:2007"

validPass =
    "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\n\
    \hcl:#623a2f\n\n\
    \eyr:2029 ecl:blu cid:129 byr:1989\n\
    \iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm\n\n\
    \hcl:#888785\n\
    \hgt:164cm byr:2001 iyr:2015 cid:88\n\
    \pid:545766238 ecl:hzl\n\
    \eyr:2022\n\n\
    \iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"