module File where


import Algorithms
import Types
import Binary
import qualified Data.ByteString as BS

encode :: String -> IO ()
encode arquivo = do
  texto <- readFile arquivo
  let huff = construirArvore $ freqSimb texto
  print "Arvore de Huffman:"
  print huff
  let encoded = bitstreamToByteString $ getBits huff texto
  writeFile (arquivo ++ ".arvore") $ show encoded
  BS.writeFile (arquivo ++ ".out") encoded


decode :: String -> String -> IO ()
decode file treeFile = do
  tree' <- readFile treeFile
  let tree = bitstreamToHuffman $ byteStringToBitstream $ read tree'
  texto <- BS.readFile file
  let decoded = getString tree $ concatMap wordToBit (BS.unpack texto)
  writeFile file decoded