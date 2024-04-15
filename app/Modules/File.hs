module File where


import Algorithms
import Types
import Data.Binary (Put, Get)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Binary.Put as Put
import Data.Char (ord, chr)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Binary as Get
import qualified Data.Binary.Get as Get
import Data.ByteString.Internal (c2w)




-- | Codifica um arquivo usando o algoritmo de Huffman.
--
-- A função `encode` recebe o caminho de um arquivo como entrada e realiza a codificação do mesmo usando o algoritmo de Huffman.
-- O arquivo é lido, e em seguida são calculadas as frequências dos símbolos presentes no texto.
-- A árvore de Huffman é construída a partir das frequências dos símbolos.
-- Em seguida, o arquivo binário é gerado com base nas informações da árvore de Huffman e das frequências dos símbolos.
-- O arquivo binário é salvo com a extensão ".bin" no mesmo diretório do arquivo original.
--
-- A função `encode` é uma função de entrada e saída (IO) que não retorna nenhum valor.
-- Ela utiliza as funções auxiliares `somarCaracteres`, `escreverArquivo`, `escreverSimbolos` e `escreverCodigos` para realizar a codificação.
encode :: String -- ^ O caminho do arquivo a ser codificado.
       -> IO ()  -- ^ Ação IO que realiza a codificação do arquivo.
encode arquivo = do
  texto <- readFile arquivo
  let freqSimbolos = freqSimb texto
  putStrLn $ "Frequencia de simbolos: "  ++ show (length freqSimbolos)
  let totalCaracteres = somarCaracteres freqSimbolos
  putStrLn $ "Total de caracteres: " ++ show totalCaracteres
  let huff = construirArvore freqSimbolos

  putStrLn $ "Frequencia de simbolos: " ++ show freqSimbolos
  putStrLn $ "Arvore de Huffman: " ++ show huff

  let arquivoBin = Put.runPut $ escreverArquivo (length freqSimbolos) totalCaracteres freqSimbolos huff
  Lazy.writeFile (arquivo ++ ".bin") arquivoBin

    where
      -- | Calcula a soma das frequências dos símbolos presentes na lista de Huffman.
      somarCaracteres :: [Huffman] -> Int
      somarCaracteres [] = 0
      somarCaracteres ((Folha freq _):hs) = freq + somarCaracteres hs
      somarCaracteres ((No freq _ _):hs) = freq + somarCaracteres hs

      -- | Escreve as informações do arquivo binário com base nas frequências dos símbolos e na árvore de Huffman.
      escreverArquivo :: Int         -- ^ O total de frequências de símbolos.
                     -> Int         -- ^ O total de caracteres no texto original.
                     -> [Huffman]   -- ^ A lista de Huffman.
                     -> Huffman     -- ^ A árvore de Huffman.
                     -> Put         -- ^ Ação Put que escreve as informações do arquivo binário.
      escreverArquivo totalFrequenciaSimbolos totalCaracteres simbolos arvore = do
        Put.putWord8 $ toEnum totalFrequenciaSimbolos
        Put.putWord32be $ toEnum totalCaracteres

        escreverSimbolos simbolos
        -- escreverCodigos arvore Map.empty

      -- | Escreve as informações dos símbolos no arquivo binário.
      escreverSimbolos :: [Huffman] -- ^ A lista de Huffman.
                       -> Put       -- ^ Ação Put que escreve as informações dos símbolos.
      escreverSimbolos [] = return ()
      escreverSimbolos ((Folha freq simbolo):hs) = do
        Put.putWord8 $ fromIntegral $ ord simbolo
        Put.putWord32be $ fromIntegral freq
        escreverSimbolos hs
      escreverSimbolos ((No {}):hs) = do
        escreverSimbolos hs

      -- | Escreve os códigos dos símbolos no arquivo binário.
      escreverCodigos :: Huffman              -- ^ A árvore de Huffman.
                      -> Map Char ByteString -- ^ O mapa de códigos dos símbolos.
                      -> Put                  -- ^ Ação Put que escreve os códigos dos símbolos.
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


decode :: String -> IO ()
decode arquivo = do
  arquivoBin <- Lazy.readFile arquivo
  let (totalFrequenciaSimbolos, totalCaracteres, simbolos) = Get.runGet decodificarBinario arquivoBin
  
  print totalFrequenciaSimbolos
  print totalCaracteres
  print simbolos
  
  where
    decodificarBinario :: Get (Get.Word8, Get.Word32, [(Int, Char)])
    decodificarBinario = do
      totalFrequenciaSimbolos <- Get.getWord8
      totalCaracteres <- Get.getWord32be
      simbolos <- lerArquivo $ fromIntegral totalFrequenciaSimbolos
      return (totalFrequenciaSimbolos, totalCaracteres, simbolos)

    lerArquivo :: Int -> Get [(Int, Char)]
    lerArquivo 0 = return []
    lerArquivo n = do
      simbolo <- Get.getWord8
      freq <- Get.getWord32be
      resto <- lerArquivo (n - 1)
      return $ (fromIntegral freq, chr $ fromIntegral simbolo) : resto
