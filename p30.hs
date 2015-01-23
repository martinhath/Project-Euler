import Data.Char
import Control.Monad

getDigits :: Int -> [Int]
getDigits n = map digitToInt $ show n

fifth :: Int -> Int
fifth = flip (^) 5

isEq :: Int -> Bool
isEq n = (sum $ map fifth $ getDigits n) == n

main :: IO ()
main = do
    mapM_ putStrLn $ map show $ filter isEq [10..]

