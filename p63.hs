nums :: Int -> Int
nums a = length $ takeWhile f [(head $ filter f [1..])..]
            where f x = a == (length . show $ (x^a))

main :: IO ()
main = do
    putStrLn $ show $ sum $ map nums [1..21] --try and fail with 21
