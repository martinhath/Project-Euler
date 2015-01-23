import qualified Data.Maybe as M

isprime :: Int -> Bool
isprime 1 = False
isprime 2 = True
isprime n = 0 == (length $ filter (\x -> (mod n x) == 0) (2:[3,5..(floor $ sqrt $ fromIntegral n)]))

takejust :: [Maybe a] -> [a]
takejust ((Nothing):xs) = []
takejust (x:xs) = (M.fromJust x):(takejust xs)

applyrepeat :: (a -> Maybe a) -> a -> [a]
applyrepeat f x = applyrepeat' f $ repeat x

applyrepeat' :: (a -> Maybe a) -> [a] -> [a]
applyrepeat' f (x:xs) = takejust $ map f (x:(applyrepeat' f xs)) 

truncleft :: Int -> Maybe Int
truncleft n
    | n < 10      = Nothing
    | otherwise   = Just $ read . tail $ show n

truncright :: Int -> Maybe Int
truncright n
    | n < 10      = Nothing
    | otherwise   = Just $ read . init $ show n

gettrunced :: Int -> [Int]
gettrunced n = n:(applyrepeat truncleft n) ++ (applyrepeat truncright n)

isonlyprimes :: [Int] -> Bool
isonlyprimes = and . map isprime

istruncatable :: Int -> Bool
istruncatable = isonlyprimes . gettrunced


main :: IO ()
main = do
    let nums = filter istruncatable $ filter isprime [10..]
    putStrLn . show $ sum $ take 11 $ nums
