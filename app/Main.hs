module Main where
import Algorithms
    ( freqSimb, construirArvore, codificar, decodificar )
import File

main :: IO ()
main = do
    print "Hello, World!"
    -- -- Codificando e decodificando com Huffman
    -- let texto = "Gustavo"

    -- let arvore = construirArvore $ freqSimb texto

    -- -- print $ arvore

    -- let textoCompactado = codificar texto $ arvore

    -- let textoDecodificado = decodificar textoCompactado $ arvore

    -- putStrLn $ "Texto original: " ++ texto

    -- putStrLn $ "Texto compactado: " ++ textoCompactado

    -- putStrLn $ "Texto decodificado: " ++ textoDecodificado


    -- Compacta o arquivo
    putStrLn "Compactando arquivo..."
    encode "teste.txt"
    putStrLn "Arquivo compactado com sucesso!"
    
    -- Descompacta o arquivo
    putStrLn "Descompactando arquivo..."
    decode "teste.txt.out" "teste.txt.arvore"
    putStrLn "Arquivo descompactado com sucesso!"