module File where

import Algorithms ( freq, convert, construirArvore, codificar )
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