module Main where

import Ciag
import Lista
import Cezar
import Vigenere
import System.IO
import System.Environment

main :: IO ()
main = do
    putStrLn "SZYFRATOR"
    putStrLn "Co chcesz zrobić?\n1 - pracować w konsoli\n2 - pracować na pliku\n"
    cmdIn <- getLine
    let cmd = read cmdIn :: Int
    case cmd of
      1 -> konsolaProg
      2 -> plikProg
      otherwise -> putStrLn "Nie rozpoznano polecenia."

konsolaProg :: IO ()
konsolaProg = do
      putStrLn "Co chcesz zrobić?\n1 - zaszyfrować\n2 - odszyfrować\n"
      cmdIn <- getLine
      let cmd = read cmdIn :: Int
      case cmd of
        1 -> konsolaSzyfr 1
        2 -> konsolaSzyfr 2
        otherwise -> putStrLn "Nie rozpoznano polecenia."

konsolaSzyfr :: Int -> IO ()
konsolaSzyfr x = do
      putStrLn "Z jakiego szyfru chcesz korzystać? [1-4]: "
      cmdIn <- getLine
      let cmd = read cmdIn :: Int
      case (x-1)*4 + cmd of
        1 -> konsolaSzyfrCiag
        2 -> konsolaSzyfrLista
        3 -> konsolaSzyfrCezar
        4 -> konsolaSzyfrVig
        5 -> konsolaDeszyfrCiag
        6 -> konsolaDeszyfrLista
        7 -> konsolaDeszyfrCezar
        8 -> konsolaDeszyfrVig
        otherwise -> putStrLn "Nie rozpoznano polecenia."

konsolaSzyfrCiag :: IO ()
konsolaSzyfrCiag = do
      putStrLn "Podaj string do zaszyfrowania: "
      line <- getLine
      putStrLn "Podaj klucz: "
      keyIn <- getLine
      putStrLn ("Wczytano ciąg: " ++ line)
      putStrLn ("Wczytano klucz: " ++ keyIn)
      let key = read keyIn :: Int
      putStr "Zaszyfrowany ciąg: "
      Ciag.wyswietlSzyfrowanie line key


konsolaSzyfrLista :: IO ()
konsolaSzyfrLista = do
      putStrLn "Podaj listę do zaszyfrowania: "
      line <- getLine
      putStrLn "Podaj klucz (jako listę): "
      keyIn <- getLine
      putStrLn ("Wczytano ciąg: " ++ line)
      putStrLn ("Wczytano klucz: " ++ keyIn)
      let key = read keyIn :: [Int]
      let lin = read line :: [Int]
      putStr "Zaszyfrowana lista: "
      wyswietlSzyfrowanieLista lin key

konsolaSzyfrCezar :: IO ()
konsolaSzyfrCezar = do
      putStrLn "Podaj ciąg do zaszyfrowania szyfrem Cezara: "
      line <- getLine
      putStrLn "Podaj klucz: "
      keyIn <- getLine
      putStrLn ("Wczytano ciąg: " ++ line)
      putStrLn ("Wczytano klucz: " ++ keyIn)
      let key = read keyIn :: Int
      putStr "Zaszyfrowany ciąg: "
      Cezar.wyswietlSzyfrowanie line key

konsolaSzyfrVig :: IO ()
konsolaSzyfrVig = do
      putStrLn "Podaj String do zaszyfrowania szyfrem Vigenera"
      line <- getLine
      putStrLn "Podaj klucz: "
      keyIn <- getLine
      putStrLn ("Wczytano ciąg: " ++ line)
      putStrLn ("Wczytano klucz: " ++ keyIn)
      if (Vigenere.sprawdzCzyDobryKlucz keyIn) then
        do
          putStr "zaszyfrowany ciąg: "
          Vigenere.wyswietlSzyfrowanie line keyIn
      else
        do
          putStrLn "Błędny klucz :("

konsolaDeszyfrCiag :: IO ()
konsolaDeszyfrCiag = do
       putStrLn "Podaj String do odszyfrowania"
       line <- getLine
       putStrLn "Podaj klucz: "
       keyIn <- getLine
       if (null keyIn) then
         do
           putStrLn "Mozliwe odszyfrowane ciagu:"
           Ciag.wyswietlPropozycje line
       else
         do
           let key = read keyIn :: Int
           putStr "Odszyfrowany ciąg: "
           Ciag.wyswietlDeszyfrowanie line key

konsolaDeszyfrLista :: IO ()
konsolaDeszyfrLista = do
      putStrLn "Podaj listę do odszyfrowania: "
      line <- getLine
      putStrLn "Podaj klucz (jako listę): "
      keyIn <- getLine
      putStrLn ("Wczytano listę: " ++ line)
      putStrLn ("Wczytano klucz: " ++ keyIn)
      let key = read keyIn :: [Int]
      let lin = read line :: [Int]
      putStr "Zaszyfrowana lista: "
      wyswietlDeszyfrowanieLista lin key

konsolaDeszyfrCezar :: IO ()
konsolaDeszyfrCezar = do
      putStrLn "Podaj ciąg do odszyfrowania szyfrem Cezara: "
      line <- getLine
      putStrLn "Podaj klucz: "
      keyIn <- getLine
      putStrLn ("Wczytano ciąg: " ++ line)
      putStrLn ("Wczytano klucz: " ++ keyIn)
      let key = read keyIn :: Int
      putStr "Odszyfrowany ciąg: "
      Cezar.wyswietlDeszyfrowanie line key

konsolaDeszyfrVig :: IO ()
konsolaDeszyfrVig = do
      putStrLn "Podaj String do odszyfrowania szyfrem Vigenera"
      line <- getLine
      putStrLn "Podaj klucz: "
      keyIn <- getLine
      putStrLn ("Wczytano ciąg: " ++ line)
      putStrLn ("Wczytano klucz: " ++ keyIn)
      if (Vigenere.sprawdzCzyDobryKlucz keyIn) then
        do
          putStr "zaszyfrowany ciąg: "
          Vigenere.wyswietlDeszyfrowanie line keyIn
      else
        do
          putStrLn "Błędny klucz :("

plikProg :: IO()
plikProg = do
      inpStr <- readFile "do_zaszyfrowania.txt"
      writeFile "zakodowane.txt" (Cezar.szyfrujCiag inpStr 12)
