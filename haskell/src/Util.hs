module Util where

import Data.Bits (Bits, setBit, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.List (foldl')
import Data.Word (Word16, Word8)

-- slices from [start, end]
slice xs from to = take (to - from + 1) (drop from xs)

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

merge :: Word8 -> Word8 -> Word16
merge l r = foldl' go 0x0 (zip [15, 14 .. 0] bits)
  where
    go acc (n, True) = setBit acc n
    go acc (n, False) = acc
    bits =
      map (testBit l) [7, 6 .. 0]
        ++ map (testBit r) [7, 6 .. 0]

-- | Combine two-byte chunks into Word16
processBits :: [Word8] -> [Word16]
processBits bytes = map go (chunks 2 bytes)
  where
    go [_] = error "odd number"
    go [x, y] = merge x y

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = do
  let (l, r) = splitAt n xs
  l : chunks n r

signExtend :: Word16 -> Int -> Word16
signExtend x bitCount
  | x `shiftR` (bitCount - 1) .&. 1 == 1 = x .|. (0xFFFF `shiftL` bitCount)
  | otherwise = x
