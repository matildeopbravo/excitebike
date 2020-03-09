
module Aula where

import System.Random
import System.IO.Error

-- randomRIO :: Random a => (a,a) -> IO a
-- tryIOError :: IO a -> IO (Either IOError a)

adivinha :: IO ()
adivinha = do x <- randomRIO (0,9)
              putStrLn "Qual é o número (0-9) que escolhi?"
              n <- jogar x 1
              putStrLn ("Acertou em "++(show n)++" tentativas.")


jogar :: Int -> Int -> IO Int
jogar x n = do s <- getLine
               r <- readIO s
               if x==r
               then return n
               else if x>r
                    then putStrLn "É baixo... tente de novo." >> jogar x (n+1)
                    else do { putStrLn "É alto... tente de novo.";
                              jogar x (n+1) 
                            }

--------------------------

guess :: IO ()
guess = do x <- randomRIO (0,9)
           putStrLn "Qual é o número (0-9) que escolhi?"
           n <- play x 1
           putStrLn ("Acertou em "++(show n)++" tentativas.")


play :: Int -> Int -> IO Int
play x n = do s <- getLine
              y <- tryIOError (readIO s)
              case y of
                Left _ -> putStrLn "Erro ..." >> play x n 
                Right r -> if x==r
                           then return n
                           else if x>r
                           then putStrLn "É baixo... tente de novo." >> play x (n+1)
                           else do { putStrLn "É alto... tente de novo.";
                                     play x (n+1) 
                                   }
