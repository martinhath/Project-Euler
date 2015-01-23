import Data.List
import Data.Char


isprime :: Int -> Bool
isprime 1 = False
isprime 2 = True
isprime n = 0 == (length $ filter (\x -> (mod n x) == 0) (2:[3,5..(floor $ sqrt $ fromIntegral n)]))

pandigital :: Int -> Bool
pandigital n = num == (map intToDigit [1..(length num)])
                where num = sort $ show n
                      

panprime :: Int -> Bool
panprime n = pandigital n && isprime n

main :: IO ()
main = do
    mapM_ (putStrLn . show) $ filter panprime [2..]
