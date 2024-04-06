module File where

import Algorithms (construirArvore, codificar, decodificar, freqSimb )
import System.IO ()

-- Função principal para compactar o conteúdo de um arquivo
compactarArquivo :: FilePath -> IO ()
compactarArquivo arquivo = do
  conteudo <- readFile arquivo
  let frequencias = freqSimb conteudo
      arvore = construirArvore frequencias
      conteudoCompactado = codificar conteudo arvore
  writeFile (arquivo ++ ".huff") conteudoCompactado
  putStrLn $ "Arquivo compactado como " ++ arquivo ++ ".huff"

-- Função principal para descompactar o conteúdo de um arquivo
descompactarArquivo :: FilePath -> IO ()
descompactarArquivo arquivo = do
  conteudoCompactado <- readFile arquivo
  let frequencias = freqSimb conteudoCompactado
      arvore = construirArvore frequencias
      conteudo = decodificar conteudoCompactado arvore
  writeFile (take (length arquivo - 5) arquivo) conteudo
  putStrLn $ "Arquivo descompactado como " ++ take (length arquivo - 5) arquivo