module Main where

import Ciag
import Lista
import Wektor

main :: IO ()

main = do {
    putStrLn "PODAJ STRING DO ZASZYFROWANIA: "
    ; line <- getLine
    ; putStrLn "PODAJ KLUCZ: "
    ; keyIn <- getLine
    ; putStrLn ("WCZYTANO CIAG: "++line)
    ; putStrLn ("WCZYTANO KLUCZ: "++keyIn)
    ; let key = read keyIn :: Int
    ; putStr "SZYFR: "
    ; wyswietlSzyfrowanie line key
    ; let l1 = cSzyfruj line key
    ; putStr "SZUKAJ CO TO: "
    ; wyswietlPropozycje l1
    }

foo = (+1)