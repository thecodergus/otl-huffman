module Binary where


import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Map as Map
import Types
import Data.Binary (Word8)

-- Função que retorna o tamanho de uma palavra em bits
wordSize :: Int
wordSize = 8


-- Função que retorna uma mapa de frequências de caracteres
getEncodings :: Huffman -> Map.Map Char [Bit]
getEncodings huff = getEncodings' huff []
  where
    getEncodings' :: Huffman -> [Bit] -> Map.Map Char [Bit]
    getEncodings' (Folha _ c) bits = Map.singleton c bits
    getEncodings' (No _ esq dir) bits =
      let esq' = getEncodings' esq (bits ++ [Zero])
          dir' = getEncodings' dir (bits ++ [One])
      in Map.union esq' dir'

-- Tranforma texto em uma sequencia de arvores codificada em bits
getBits :: Huffman -> String -> [Bit]
getBits huff = concatMap (\c -> getEncodings huff Map.! c)


getString :: Huffman -> [Bit] -> String
getString huff bits = getString' huff bits []
  where
    getString' :: Huffman -> [Bit] -> String -> String
    getString' huff' bits' acc
      | null bits' = acc
      | otherwise =
        let (c, bits'') = getChar' huff' bits'
        in getString' huff' bits'' (acc ++ [c])

    getChar' :: Huffman -> [Bit] -> (Char, [Bit])
    getChar' (Folha _ c) bits' = (c, bits')
    getChar' (No _ esq _) (Zero:bits') = getChar' esq bits'
    getChar' (No _ _ dir) (One:bits') = getChar' dir bits'
    getChar' _ _ = error "Invalid input"


bitToWord :: [Bit] -> Word8
bitToWord bits = bitToWord' bits 0
  where
    bitToWord' :: [Bit] -> Word8 -> Word8
    bitToWord' [] acc = acc
    bitToWord' (b:bs) acc
        | b == One = bitToWord' bs (acc * 2 + 1)
        | otherwise = bitToWord' bs (acc * 2)

wordToBit :: Word8 -> [Bit]
wordToBit w = wordToBit' w []
  where
    wordToBit' :: Word8 -> [Bit] -> [Bit]
    wordToBit' 0 acc = replicate (wordSize - length acc) Zero ++ acc
    wordToBit' w' acc
        | w' `mod` 2 == 1 = wordToBit' (w' `div` 2) (One:acc)
        | otherwise = wordToBit' (w' `div` 2) (Zero:acc)


extendToMultipleOfWordSize :: [Bit] -> [Bit]
extendToMultipleOfWordSize bits
    | length bits `mod` wordSize == 0 = bits
    | otherwise = bits ++ replicate (wordSize - length bits `mod` wordSize) Zero

bitstreamToByteString :: [Bit] -> BS.ByteString
bitstreamToByteString bits = bsts (extendToMultipleOfWordSize bits) []
  where
    bsts [] ws = BS.pack ws
    bsts b s =
        bsts (drop wordSize b) (s ++ [bitToWord (take wordSize b)])