module File where


import Algorithms
import Types
import Data.Binary (Put, Get)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Binary.Put as Put
import Data.Char (ord)
import qualified Data.Map (Map)
import Data.ByteString (ByteString)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Map as Map
import Data.Map (Map)




encode :: String -> IO ()
encode arquivo = do
  texto <- readFile arquivo
  let freqSimbolos = freqSimb texto
  let totalCaracteres = somarCaracteres freqSimbolos
  let huff = construirArvore freqSimbolos

  let arquivoBin = Put.runPut $ escreverArquivo (length freqSimbolos) totalCaracteres freqSimbolos huff
  Lazy.writeFile (arquivo ++ ".bin") arquivoBin

    where
      somarCaracteres :: [Huffman] -> Int
      somarCaracteres [] = 0
      somarCaracteres ((Folha freq _):hs) = freq + somarCaracteres hs
      somarCaracteres ((No freq _ _):hs) = freq + somarCaracteres hs

      escreverArquivo :: Int -> Int -> [Huffman] -> Huffman -> Put
      escreverArquivo totalFrequenciaSimbolos totalCaracteres [] arvore = do
        Put.putWord8 $ fromIntegral totalFrequenciaSimbolos
        Put.putWord32be $ fromIntegral totalCaracteres
        
        escreverCodigos arvore Map.empty
      
      escreverArquivo totalFrequenciaSimbolos totalCaracteres simbolos _ = do
        Put.putWord8 $ fromIntegral totalFrequenciaSimbolos
        Put.putWord32be $ fromIntegral totalCaracteres

        escreverSimbolos simbolos
          
      escreverSimbolos :: [Huffman] -> Put
      escreverSimbolos [] = return ()
      escreverSimbolos ((Folha freq simbolo):hs) = do
        Put.putWord8 $ fromIntegral $ ord simbolo
        Put.putWord32be $ fromIntegral freq
        escreverSimbolos hs
      escreverSimbolos ((No {}):hs) = do
        escreverSimbolos hs

      escreverCodigos :: Huffman -> Map Char ByteString -> Put
      escreverCodigos (Folha _ simbolo) codigos = do
        let codigo = Map.findWithDefault (error "Símbolo não encontrado") simbolo codigos
        Put.putWord8 $ fromIntegral $ ord simbolo
        Put.putWord8 $ fromIntegral $ ByteString.length codigo
        Put.putByteString codigo
      escreverCodigos (No _ (Folha _ simbolo) direita) codigos = do
        let novosCodigos = Map.insertWith (<>) simbolo (ByteString.singleton $ fromIntegral $ ord '0') codigos
        escreverCodigos direita novosCodigos
        escreverCodigos direita novosCodigos
      escreverCodigos (No _ esquerda (Folha _ simbolo)) codigos = do
        let novosCodigos = Map.insertWith (<>) simbolo (ByteString.singleton $ fromIntegral $ ord '1') codigos
        escreverCodigos esquerda novosCodigos
        escreverCodigos esquerda novosCodigos
      escreverCodigos (No _ esquerda direita) codigos = do
        escreverCodigos esquerda codigos
        escreverCodigos direita codigos