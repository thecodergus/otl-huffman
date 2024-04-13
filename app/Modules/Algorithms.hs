module Algorithms where

import Types(Huffman (Folha, No))
import Data.Map (toList, fromListWith)
import Data.List ( insertBy, sortBy )
import Data.Function ( on )
import Control.Arrow(second)
import Data.Maybe(fromMaybe)


-- Calcula a frequência dos símbolos em uma string e converte para uma lista de Huffman
freqSimb :: String -> [Huffman]
freqSimb = convert . freq
  where
    -- Calcula a frequência dos caracteres em uma string
    freq :: String -> [(Char, Int)]
    freq = toList . fromListWith (+) . map (flip (,) 1)

    -- Converte uma lista de tuplas (caractere, frequência) em uma lista de Huffman
    convert :: [(Char, Int)] -> [Huffman]
    convert = map (\(a, b) -> Folha b a) . sortBy (compare `on` snd)


-- Constrói uma árvore Huffman a partir de uma lista de Huffman
construirArvore :: [Huffman] -> Huffman
construirArvore [] = error "Lista vazia"
construirArvore [t] = t
construirArvore (a : b : cs) = construirArvore $ insertBy (compare `on` weight) (No (weight a + weight b) a b) cs
  where
    -- Obtém o peso de um nó Huffman
    weight :: Huffman -> Int
    weight (Folha w _) = w
    weight (No w _ _) = w


-- Mapeia os caracteres para seus códigos Huffman
codHumman :: Huffman -> [(Char, String)]
codHumman (Folha _ c) = [(c, "")]
codHumman (No _ e d) = map (second ('0' :)) (codHumman e) ++ map (second ('1' :)) (codHumman d)

-- Codifica uma string usando a árvore Huffman
codificar :: String -> Huffman -> String
codificar s h = concatMap (`lookup'` cod) s
  where
    -- Mapeia os caracteres para seus códigos Huffman
    cod = codHumman h
    
    -- Função complementar para fazer a busca de um caractere no mapeamento
    lookup' c = fromMaybe (error "Caractere não encontrado") . lookup c

-- Decodifica uma string usando a árvore Huffman
decodificar :: String -> Huffman -> String
decodificar str arv = decodificar' str arv
  where
    -- Função complementar para decodificar uma string usando a árvore Huffman
    -- esta função é recursiva e é chamada pela função decodificar
    -- ela percorre a árvore Huffman de acordo com a string de entrada
    -- e retorna a string decodificada
    decodificar' :: String -> Huffman -> String
    decodificar' ('0' : xstr) (No _ esq _) = decodificar' xstr esq
    decodificar' ('1' : xstr) (No _ _ dir) = decodificar' xstr dir
    decodificar' [] (Folha _ c) = [c]
    decodificar' str' (Folha _ c) = c : decodificar' str' arv
    decodificar' _ _ = error "Input Invalido"
    

