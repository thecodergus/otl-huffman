module File where

import Algorithms ( freq, convert, construirArvore, codificar, decodificar )
import System.IO ()

-- Função principal para compactar o conteúdo de um arquivo
compactarArquivo :: FilePath -> IO ()
compactarArquivo arquivo = do
  conteudo <- readFile arquivo
  let frequencias = freq conteudo
      arvore = construirArvore (convert frequencias)
      conteudoCompactado = codificar conteudo arvore
  writeFile (arquivo ++ ".huff") conteudoCompactado
  putStrLn $ "Arquivo compactado como " ++ arquivo ++ ".huff"

-- Função principal para descompactar o conteúdo de um arquivo
descompactarArquivo :: FilePath -> IO ()
descompactarArquivo arquivo = do
  conteudoCompactado <- readFile arquivo
  let frequencias = freq conteudoCompactado
      arvore = construirArvore (convert frequencias)
      conteudo = decodificar conteudoCompactado arvore
  writeFile (take (length arquivo - 5) arquivo) conteudo
  putStrLn $ "Arquivo descompactado como " ++ take (length arquivo - 5) arquivo