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
    cod = codHumman h
    lookup' c = fromMaybe (error "Caractere não encontrado") . lookup c

-- Decodifica uma string usando a árvore Huffman
decodificar :: String -> Huffman -> String
decodificar s h = decodificar' s h h
  where
    decodificar' :: String -> Huffman -> Huffman -> String
    decodificar' [] _ _ = []
    decodificar' s' h' (No _ e d) = case s' of
      ('0' : ss) -> decodificar' ss h' e
      ('1' : ss) -> decodificar' ss h' d
      _ -> error "Código inválido"
    decodificar' s' h' (Folha _ c) = c : decodificar' s' h' h'