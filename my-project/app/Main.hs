module Main where

import Ciag
import Lista
import Cezar
import Vigenere

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
    ; Ciag.wyswietlSzyfrowanie line key
    ; let l1 = cSzyfruj line key
    ; putStr "SZUKAJ CO TO: "
    ; wyswietlPropozycje l1
    ; putStrLn "_____________ "

    ; putStrLn "PODAJ LISTĘ DO ZASZYFROWANIA: "
    ; line <- getLine
    ; putStrLn "PODAJ KLUCZ (jako listę): "
    ; keyIn <- getLine
    ; putStrLn ("WCZYTANO CIAG: "++line)
    ; putStrLn ("WCZYTANO KLUCZ: "++keyIn)
    ; let key = read keyIn :: [Int]
    ; let lin = read line :: [Int]
    ; putStr "SZYFR: "
    ; wyswietlSzyfrowanieLista lin key
    }

foo = (+1)
