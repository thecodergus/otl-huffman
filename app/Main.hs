module Main where
-- import File (descompactarArquivo, compactarArquivo)
import Algorithms
import Algorithms (codificar)

main :: IO ()
main = do

    -- Codificando e decodificando com Huffman
    let texto = "Gustavo Michels de Camargo"

    let arvore = construirArvore $ freqSimb texto

    let textoCompactado = codificar texto $ arvore

    let textoDecodificado = decodificar textoCompactado $ arvore

    putStrLn $ "Texto original: " ++ texto

    putStrLn $ "Texto compactado: " ++ textoCompactado

    putStrLn $ "Texto decodificado: " ++ textoDecodificado

    
    -- -- Compacta o arquivo
    -- putStrLn "Compactando arquivo..."
    -- compactarArquivo "teste.txt"
    -- putStrLn "Arquivo compactado com sucesso!"
    
    -- -- Descompacta o arquivo
    -- putStrLn "Descompactando arquivo..."
    -- descompactarArquivo "teste.txt.huff"
    -- putStrLn "Arquivo descompactado com sucesso!"