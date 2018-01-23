module Main where

import Lib
import Ciag
import Lista

main :: IO ()
main = do {
    putStrLn "PODAJ CIAG DO ZASZYFROWANIA: "
    ; line <- getLine
    ; putStrLn "PODAJ KLUCZ: "
    ; keyIn <- getLine
    ; putStrLn ("WCZYTANO CIAG: "++line)
    ; let key = read keyIn :: Int
    ; putStrLn ("WCZYTANO KLUCZ: "++keyIn)
    ; putStr "SZYFR: "
    ; wyswietlSzyfrowanie line key
    ; let l1 = cSzyfruj line key
    ; putStr "SZUKAJ CO TO: "
    ; wyswietlPropozycje l1
    }

foo = (+1)