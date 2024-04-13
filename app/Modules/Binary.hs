module Binary where


import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Types
import Data.Binary (Word8)

-- Função que retorna o tamanho de uma palavra em bits
wordSize :: Int
wordSize = 8

-- Dada uma árvore de Huffman, retorna um mapeamento de cada caractere para sua representação em bits.
getEncodings :: Huffman -> Map.Map Char [Bit]
getEncodings huff = getEncodings' huff []
  where
    -- Função auxiliar para getEncodings que percorre a árvore de Huffman e constrói o mapeamento.
    getEncodings' :: Huffman -> [Bit] -> Map.Map Char [Bit]
    -- Caso base: adiciona um mapeamento de caractere para bits para uma folha.
    getEncodings' (Folha _ c) bits = Map.singleton c bits
    getEncodings' (No _ esq dir) bits =
      let esq' = getEncodings' esq (bits ++ [Zero])
          dir' = getEncodings' dir (bits ++ [One])
      in Map.union esq' dir'
    -- Caso recursivo: percorre as subárvores esquerda e direita, adicionando um zero ou um para cada nível.

-- Dada uma árvore de Huffman e uma string, retorna a representação em bits da string usando a codificação de Huffman.
getBits :: Huffman -> String -> [Bit]
getBits huff = concatMap (\c -> getEncodings huff Map.! c)

-- Dada uma árvore de Huffman e uma lista de bits, decodifica a lista de bits em uma string usando a codificação de Huffman.
getString :: Huffman -> [Bit] -> String
getString huff bits = getString' huff bits []
  where
    -- Função auxiliar para getString que percorre a árvore de Huffman e decodifica a lista de bits.
    getString' :: Huffman -> [Bit] -> String -> String
    getString' huff' bits' acc
      | null bits' = acc
      -- Caso base: quando não há mais bits para decodificar, retorna a string acumulada.
      | otherwise =
        let (c, bits'') = getChar' huff' bits'
        -- Decodifica o próximo caractere e os bits restantes usando a função auxiliar getChar'.
        in getString' huff' bits'' (acc ++ [c])
    -- Chama recursivamente getString' com os bits restantes e o caractere decodificado adicionado à string acumulada.

    -- Função auxiliar para getString' que decodifica o próximo caractere na lista de bits.
    getChar' :: Huffman -> [Bit] -> (Char, [Bit])
    getChar' (Folha _ c) bits' = (c, bits')
    -- Caso base: quando uma folha é encontrada, retorna o caractere e os bits restantes.
    getChar' (No _ esq _) (Zero:bits') = getChar' esq bits'
    -- Caso recursivo: quando um nó interno é encontrado com um bit zero, percorre a subárvore esquerda.
    getChar' (No _ _ dir) (One:bits') = getChar' dir bits'
    -- Caso recursivo: quando um nó interno é encontrado com um bit um, percorre a subárvore direita.
    getChar' _ _ = error "Invalid input"
    -- Caso inválido: quando a lista de bits não corresponde a uma codificação de Huffman válida.

-- Converte uma lista de bits em um único byte (Word8).
bitToWord :: [Bit] -> Word8
bitToWord bits = bitToWord' bits 0
  where
    bitToWord' :: [Bit] -> Word8 -> Word8
    -- Função auxiliar para bitToWord que converte os bits em um byte usando aritmética binária.
    bitToWord' [] acc = acc
    -- Caso base: quando não há mais bits para processar, retorna o byte acumulado.
    bitToWord' (b:bs) acc
        | b == One = bitToWord' bs (acc * 2 + 1)
        -- Caso recursivo: quando o bit atual é um, adiciona-o ao byte acumulado e processa os bits restantes.
        | otherwise = bitToWord' bs (acc * 2)
    -- Caso recursivo: quando o bit atual é zero, adiciona-o ao byte acumulado e processa os bits restantes.

-- Converte um único byte (Word8) em uma lista de bits.
wordToBit :: Word8 -> [Bit]
wordToBit w = wordToBit' w []
  where
    wordToBit' :: Word8 -> [Bit] -> [Bit]
    -- Função auxiliar para wordToBit que converte o byte em bits usando aritmética binária.
    wordToBit' 0 acc = replicate (wordSize - length acc) Zero ++ acc
    -- Caso base: quando o byte for zero, preenche os bits restantes com zeros e retorna a lista de bits acumulada.
    wordToBit' w' acc
        | w' `mod` 2 == 1 = wordToBit' (w' `div` 2) (One:acc)
    -- Caso recursivo: quando o bit menos significativo do byte for um, adiciona-o à lista de bits acumulada e processa o byte restante.
        | otherwise = wordToBit' (w' `div` 2) (Zero:acc)
    -- Caso recursivo: quando o bit menos significativo do byte for zero, adiciona-o à lista de bits acumulada e processa o byte restante.

-- Estende uma lista de bits para um múltiplo do tamanho da palavra (8 bits) adicionando zeros à direita.
extendToMultipleOfWordSize :: [Bit] -> [Bit]
extendToMultipleOfWordSize bits
    | length bits `mod` wordSize == 0 = bits
    -- Caso base: quando o tamanho da lista de bits já é um múltiplo do tamanho da palavra, retorna a lista de bits.
    | otherwise = bits ++ replicate (wordSize - length bits `mod` wordSize) Zero
    -- Caso recursivo: adiciona zeros à direita até que o tamanho da lista de bits seja um múltiplo do tamanho da palavra.

-- Converte uma lista de bits em uma ByteString.
bitstreamToByteString :: [Bit] -> BS.ByteString
bitstreamToByteString bits = bsts (extendToMultipleOfWordSize bits) []
  where
    -- Função auxiliar para bitstreamToByteString que converte a lista de bits em uma ByteString usando a função bitToWord.
    bsts [] ws = BS.pack ws
    -- Caso recursivo: processa os bits em grupos de wordSize e converte cada grupo em um byte usando bitToWord.
    bsts b s =
        bsts (drop wordSize b) (s ++ [bitToWord (take wordSize b)])

-- Converte uma ByteString em uma lista de bits.
byteStringToBitstream :: BS.ByteString -> [Bit]
byteStringToBitstream bs = concatMap wordToBit (BS.unpack bs)

-- Convertendo uma lista de bits em uma string de 0s e 1s
bitstreamToString :: [Bit] -> String
bitstreamToString = map bitToChar
  where
    bitToChar Zero = '0'
    bitToChar One = '1'

-- Converte Bitstream em uma arvore de Huffman
bitstreamToHuffman :: [Bit] -> Huffman
bitstreamToHuffman = read . bitstreamToString