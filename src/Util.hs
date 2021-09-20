module Util where

import Data.Bits (Bits, xor)
import Data.Word (Word16)

-- slices from [start, end]
slice :: (Integral a1, Enum a1, Ord a1) => [a2] -> a1 -> a1 -> [a2]
slice arr start end = [x | start <= end, (x, j) <- zip arr [1 .. end]]

fromBits :: [Bool] -> Word16
fromBits b = fromIntegral . sum . fmap (\(pow, shouldExp) -> if shouldExp then 2 ^ pow else 0) $ zip [0 .. length b] b

update :: [a] -> Int -> a -> [a]
update ls idx val = zipWith (\i old -> (if idx == i then val else old)) [0 .. length ls] ls

toWord :: (Num b) => [Bool] -> b
toWord = fromIntegral . fromBits

lowerMask :: Integral a => a
lowerMask = 2 ^ 8 - 1

upperMask :: (Integral a, Bits a) => a
upperMask = 2 ^ 16 - 1 `xor` lowerMask