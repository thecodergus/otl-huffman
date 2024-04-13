module Types where

data Huffman
  = Folha Int Char
  | No Int Huffman Huffman
  deriving (Show, Eq, Read)

-- Função para comparar dois nós Huffman com base em sua frequência
compararHuffman :: Huffman -> Huffman -> Ordering
compararHuffman (Folha freq1 _) (Folha freq2 _) = compare freq1 freq2
compararHuffman (No freq1 _ _) (No freq2 _ _) = compare freq1 freq2
compararHuffman (Folha _ _) (No {}) = LT
compararHuffman (No {}) (Folha _ _) = GT

-- Definindo uma instância de Ord para Huffman
instance Ord Huffman where
  compare (Folha _ c1) (Folha _ c2)
    | c1 == c2 = compare c1 c2
    | otherwise = compare c1 c2
  compare (Folha w1 _) (No w2 _ _)
    | w1 == w2 = LT
    | otherwise = compare w1 w2
  compare (No w1 _ _) (Folha w2 _)
    | w1 == w2 = GT
    | otherwise = compare w1 w2
  compare (No w1 _ _) (No w2 _ _) =
    compare w1 w2

data Bit = Zero | One deriving (Show, Eq)