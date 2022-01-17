-- José Antônio Alves Maciel (jaam)

-- OBS: Para rodar o arquivo, é necessário compila-lo com o seguinte comando: "ghc --make -rtsopts -threaded JoseAntonioAlvesMaciel_q2.hs"
-- Em seguida, para executar, use: "./JoseAntonioAlvesMaciel_q2 main +RTS -N2" e depois indique com um inteiro quantos clientes
-- usarão a máquina.

-- A ideia é criar uma thread para cada cliente, e cada um deles deseja cada um dos três tipos de refrigerante.

-- A função "waitThreads" tem como objetivo a manutenção da main quando o compilado é executado. Sua implementação foi tirada
-- dos slides oferecidos na disciplina.

module Main where
import Control.Concurrent
import Control.Concurrent.MVar

maquinaRefri :: MVar Integer -> MVar Integer -> MVar Integer -> String -> Integer -> IO()
maquinaRefri cola norte quate player refri
    = do 
        value <- takeMVar refrigerante
        if (value >= 1000) then do
            putStrLn("O cliente " ++ player ++ " do refrigerante " ++ (nomeRefri refri) ++ " esta enchendo seu copo")
            threadDelay 1000000
            putMVar refrigerante (value - 300)
            valueAux <- readMVar refrigerante
            if(valueAux < 1000) then
                maquinaRefri cola norte quate player (refri)
            else
                maquinaRefri cola norte quate player (refri + 1)
        else do
            putStrLn("O refrigerante " ++ (nomeRefri refri) ++ " foi reabastecido com 1000ml, e agora possui " ++ show(value + 1000) ++ " ml")
            threadDelay 1500000
            putMVar refrigerante (value + 1000)
            maquinaRefri cola norte quate player (refri + 1)
        where
            refrigerante
             | mod refri 3 == 0 = cola
             | mod refri 3 == 1 = norte
             | mod refri 3 == 2 = quate

qntdInicial :: Integer
qntdInicial = 2000

nomeRefri :: Integer -> String
nomeRefri x
 | mod x 3 == 0 = "P-Cola"
 | mod x 3 == 1 = "Guarana Polo Norte"
 | mod x 3 == 2 = "Guarana Quate"

begin :: Integer -> MVar Integer -> MVar Integer -> MVar Integer -> IO ThreadId
begin x qntdCola qntdNorte qntdQuate 
 | x == 1    = forkIO (maquinaRefri qntdCola qntdNorte qntdQuate  (show x) 0)
 | otherwise = do
                forkIO (maquinaRefri qntdCola qntdNorte qntdQuate  (show x) 0)
                begin (x-1) qntdCola qntdNorte qntdQuate 


waitThreads :: MVar Int -> IO ()
waitThreads fim = do 
    f <- takeMVar fim
    if (f > 0) then do 
        putMVar fim f
        waitThreads fim
    else
        return ()


main::IO()
main = do
        qntdJogadores <- getLine

        fim <- newMVar (read qntdJogadores)

        qntdCola <- newMVar qntdInicial
        qntdNorte <- newMVar qntdInicial
        qntdQuate <- newMVar qntdInicial

        begin (read qntdJogadores) qntdCola qntdNorte qntdQuate 

        waitThreads fim
        return ()