import Data.Char

getDigits :: Int -> [Int]
getDigits n = map digitToInt $ show n

end :: Int -> Int
end 1 = 1
end 89 = 89
end n = end $ sum $ map (\n -> n^2) $ getDigits n

main :: IO ()
main = do
    putStrLn $ show $ length $ filter (\n -> end n == 89) [1..9999999]
