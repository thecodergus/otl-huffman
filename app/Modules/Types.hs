module Types where

data Huffman = Folha Int Char | No Int Huffman Huffman deriving (Show, Eq)



-- Função para comparar dois nós Huffman com base em sua frequência
compararHuffman :: Huffman -> Huffman -> Ordering
compararHuffman (Folha freq1 _) (Folha freq2 _) = compare freq1 freq2
compararHuffman (No freq1 _ _) (No freq2 _ _) = compare freq1 freq2
compararHuffman (Folha _ _) (No {}) = LT
compararHuffman (No {}) (Folha _ _) = GT

-- Definindo uma instância de Ord para Huffman
instance Ord Huffman where
  compare = compararHuffman