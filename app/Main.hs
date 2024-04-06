module Main where
import File (descompactarArquivo, compactarArquivo)

main :: IO ()
main = do
    -- Compacta o arquivo
    putStrLn "Compactando arquivo..."
    compactarArquivo "teste.txt"
    putStrLn "Arquivo compactado com sucesso!"
    
    -- Descompacta o arquivo
    putStrLn "Descompactando arquivo..."
    descompactarArquivo "teste.huff"
    putStrLn "Arquivo descompactado com sucesso!"
    


