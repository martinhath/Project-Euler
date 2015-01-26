import Data.List
import Data.Char

digits :: Int -> String
digits = sort . show

apply :: [(a -> b)] -> a -> [b]
apply [] _ = []
apply (x:xs) n = (x n):(apply xs n)

fapply :: (a -> b -> c) -> [a] -> [b -> c]
fapply _ [] = []
fapply f (x:xs) = (f x):(fapply f xs)

multiples :: Int -> [Int]
multiples n = apply (fapply (*) [2..6]) n

eqlist :: (Ord a) => [a] -> Bool
eqlist [x] = True
eqlist (x:s:xs) = if x == s 
                    then eqlist (s:xs)
                    else False

eqdigits :: [Int] -> Bool
eqdigits = eqlist . map digits 

main :: IO ()
main = do
    putStrLn $ show $ head $ filter (eqdigits . multiples) [1..]

