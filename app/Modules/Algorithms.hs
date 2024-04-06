module Algorithms where

import Types(Huffman (Folha, No))
import Data.Map (toList, fromListWith)
import Data.List
import Data.Function
import Control.Arrow
import Data.Maybe


freqSimb :: String -> [Huffman]
freqSimb =  convert . freq

freq :: String -> [(Char, Int)]
freq = toList . fromListWith (+) . map (flip (,) 1)

convert :: [(Char, Int)] -> [Huffman]
convert = map (\(a, b) -> Folha b a) . sortBy (compare `on` snd)

weight :: Huffman -> Int
weight (Folha w _) = w
weight (No w _ _) = w

construirArvore :: [Huffman] -> Huffman
construirArvore [] = error "Lista vazia"
construirArvore [t] = t
construirArvore (a : b : cs) = construirArvore $ insertBy (compare `on` weight) (No (weight a + weight b) a b) cs

codHumman :: Huffman -> [(Char, String)]
codHumman (Folha _ c) = [(c, "")]
codHumman (No _ e d) = map (second ('0':)) (codHumman e) ++ map (second ('1':)) (codHumman d)

codificar :: String -> Huffman -> String
codificar s h = concatMap (`lookup'` cod) s
  where
    cod = codHumman h
    lookup' c = fromMaybe (error "Caractere não encontrado") . lookup c

decodificar :: String -> Huffman -> String
decodificar s h = decodificar' s h h
  where
    decodificar' :: String -> Huffman -> Huffman -> String
    decodificar' [] _ _ = []
    decodificar' s' h' (No _ e d) = case s' of
      ('0':ss) -> decodificar' ss h' e
      ('1':ss) -> decodificar' ss h' d
      _ -> error "Código inválido"
    decodificar' s' h' (Folha _ c) = c : decodificar' s' h' h'