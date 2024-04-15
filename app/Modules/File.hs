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
  putStrLn $ "(encode)Frequencia de simbolos: "  ++ show (length freqSimbolos)
  let totalCaracteres = somarCaracteres freqSimbolos
  putStrLn $ "(encode)Total de caracteres: " ++ show totalCaracteres
  let huff = construirArvore freqSimbolos

  putStrLn $ "(encode)Frequencia de simbolos: " ++ show freqSimbolos
  putStrLn $ "(encode)Arvore de Huffman: " ++ show huff

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
        escreverCodigos arvore (gerarCodigos arvore)

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
      escreverCodigos :: Huffman -> Map Char ByteString -> Put
      escreverCodigos (Folha _ simbolo) codigos = do
        let codigo = Map.findWithDefault (error "Símbolo não encontrado") simbolo codigos
        Put.putWord8 $ fromIntegral $ ord simbolo
        Put.putWord8 $ fromIntegral $ ByteString.length codigo
        Put.putByteString codigo
      escreverCodigos (No _ esquerda direita) codigos = do
        let codigosEsquerda = Map.map (ByteString.cons $ fromIntegral $ ord '0') codigos
        let codigosDireita = Map.map (ByteString.cons $ fromIntegral $ ord '1') codigos
        escreverCodigos esquerda codigosEsquerda
        escreverCodigos direita codigosDireita

      gerarCodigos :: Huffman -> Map Char ByteString
      gerarCodigos huff = gerarCodigos' huff ByteString.empty
        where
          gerarCodigos' (Folha _ simbolo) codigo = Map.singleton simbolo codigo
          gerarCodigos' (No _ esquerda direita) codigo =
            Map.union (gerarCodigos' esquerda (ByteString.append codigo (ByteString.singleton $ c2w '0')))
                      (gerarCodigos' direita (ByteString.append codigo (ByteString.singleton $ c2w '1')))




decode :: String -> IO ()
decode arquivo = do
  arquivoBin <- Lazy.readFile arquivo
  let (totalFrequenciaSimbolos, totalCaracteres, simbolos, huff) = Get.runGet decodificarBinario arquivoBin

  putStrLn $ "(decode)Total de frequência de símbolos: " ++ show totalFrequenciaSimbolos
  putStrLn $ "(decode)Total de caracteres: " ++ show totalCaracteres
  putStrLn $ "(decode)Frequência de símbolos: " ++ show simbolos
  putStrLn $ "(decode)Arvore de Huffman: " ++ show huff

  let textoDecodificado = decodificarTexto (fromIntegral totalCaracteres) huff arquivoBin

  putStrLn $ "Texto decodificado: " ++ textoDecodificado

  Lazy.writeFile (arquivo ++ ".txt") $ Lazy.pack $ map (fromIntegral . ord) textoDecodificado

  where
    decodificarBinario :: Get (Get.Word8, Get.Word32, [(Int, Char)], Huffman)
    decodificarBinario = do
      totalFrequenciaSimbolos <- Get.getWord8
      totalCaracteres <- Get.getWord32be
      simbolos <- lerArquivo $ fromIntegral totalFrequenciaSimbolos
      (huff, _) <- lerCodigos -- Adicionado aqui

      return (totalFrequenciaSimbolos, totalCaracteres, simbolos, huff)

    lerArquivo :: Int -> Get [(Int, Char)]
    lerArquivo 0 = return []
    lerArquivo n = do
      simbolo <- Get.getWord8
      freq <- Get.getWord32be
      resto <- lerArquivo (n - 1)
      return $ (fromIntegral freq, chr $ fromIntegral simbolo) : resto


    -- \| Lê os códigos dos símbolos do arquivo binário.
    lerCodigos :: Get (Huffman, Map Char ByteString)
    lerCodigos = do
      simbolo <- Get.getWord8
      tamanhoCodigo <- Get.getWord8
      codigo <- Get.getByteString $ fromIntegral tamanhoCodigo
      let huff = Folha (ByteString.length codigo) (chr $ fromIntegral simbolo)
      let codigos = Map.singleton (chr $ fromIntegral simbolo) codigo
      if simbolo == 0 && tamanhoCodigo == 0
        then return (huff, codigos)
        else do
          (huffResto, codigosResto) <- lerCodigos
          return (No (weight huff + weight huffResto) huff huffResto, Map.union codigos codigosResto)
      where
        weight :: Huffman -> Int
        weight (Folha w _) = w
        weight (No w _ _) = w

    decodificarTexto :: Int -> Huffman -> Lazy.ByteString -> String
    decodificarTexto totalCaracteres huff arquivoBin = decodificarTexto' totalCaracteres huff arquivoBin
      where
        decodificarTexto' :: Int -> Huffman -> Lazy.ByteString -> String
        decodificarTexto' 0 _ _ = ""
        decodificarTexto' n huff' arquivoBin' = case decodificarCaractere huff' arquivoBin' of
          (caractere, resto) -> caractere : decodificarTexto' (n - 1) huff' resto

    decodificarCaractere :: Huffman -> Lazy.ByteString -> (Char, Lazy.ByteString)
    decodificarCaractere (Folha _ simbolo) arquivoBin = (simbolo, arquivoBin)
    decodificarCaractere (No _ esquerda direita) arquivoBin = case Lazy.head arquivoBin of
      0 -> decodificarCaractere esquerda (Lazy.tail arquivoBin)
      1 -> decodificarCaractere direita (Lazy.tail arquivoBin)
      _ -> error "Caractere inválido"