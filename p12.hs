import Data.List

intsqrt :: Int -> Int
intsqrt n = floor $ sqrt $ fromIntegral n


tri :: Int -> Int 
tri 1 = 1
tri n = n + tri (n-1) 

removeDup :: (Ord a) => [a] -> [a]
removeDup l = rmdup $ sort l

rmdup :: (Ord a) => [a] -> [a]
rmdup [] = []
rmdup [x] = [x]
rmdup (x:s:xs) = if (x == s)
                    then rmdup (s:xs)
                    else x:(rmdup (s:xs))


divisors :: Int -> [Int]
divisors n = foldl f [] [1..(intsqrt n)]
             where f    = \l x -> if (mod n x == 0)
                                  then [x]++[quot n x]++l
                                  else l
divisors' :: Int -> [Int]
divisors' n = filter (\x -> (mod n x) == 0) [1..n]

ans :: Int -> Int
ans len = fst $ head $ filter (\(_, ds) -> length ds > len) divs
        where divs = map (\x -> (tri x, divisors $ tri x)) [1..]


main :: IO ()
main = do
--     putStrLn $ show $ removeDup $ divisors 
    putStrLn $ show $ ans 500
