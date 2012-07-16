-- main.hs
import qualified Data.Char

main = readFile "data/data.txt" >>= writeFile "data/out0.txt" . unlines . analy

analy :: String->[String]
analy ss = zipWith (\x y->y++" : "++x)  (lines ss) . map show . map calTime . lines $ ss

calTime :: String->Int
calTime = product . map read . filter (/="") . map pickTime . words
    where
        pickTime :: String->String
        pickTime cs | cs==""         = []
                    | (head cs=='<') = tail cs
                    | otherwise      = pickTime . tail $ cs
