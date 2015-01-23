divisors :: Int -> [Int]
divisors n = filter (\x -> rem n x == 0) [1..n-1]


d :: Int -> Int
d = sum . divisors

isamicable :: Int -> Bool
isamicable n = ((d $ d n) == n) && ((d n) /= n)

main :: IO()
main = do
    let amicable = filter isamicable [1..9999]
    putStrLn $ show $ sum amicable

