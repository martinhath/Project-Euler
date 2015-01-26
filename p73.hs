import Data.List
import Data.Function

removedups :: (Ord a) => [a] -> [a]
removedups [] = []
removedups [x] = [x]
removedups (x:s:xs) = if x == s
                        then removedups (s:xs)
                        else x:(removedups (s:xs))

divi :: Int -> Int -> Double
divi = (/) `on` fromIntegral

fractions ::  Int -> [Double]
fractions 1 = []
fractions n = ([divi m n | m<-[1..n-1], gcd m n == 1]) ++ (fractions (n-1))

filter2 :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filter2 f g = filter f . filter g

main :: IO ()
main = do
    putStrLn $ show $ length $ filter2 ((>) (1/2)) ((<) (1/3)) $ fractions 12000
