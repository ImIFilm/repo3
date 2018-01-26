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
    putStrLn "Co chcesz zrobić?\n1 - pracować w konsoli\n2 - pracować na pliku"
    trybIn <- getLine
    let tryb = read trybIn :: Int
    putStrLn "Co chcesz zrobić?\n1 - zaszyfrować\n2 - odszyfrować"
    cmdIn <- getLine
    let cmd = read cmdIn :: Int
    putStrLn "Z jakiego szyfru chcesz korzystać? [1-4]: "
    szyfrIn <- getLine
    let szyfr = read szyfrIn :: Int
    case tryb of
      1 -> case cmd of
           1 -> konsolaProg cmd szyfr
           2 -> konsolaProg cmd szyfr
           otherwise -> putStrLn "Nie rozpoznano polecenia."
      2 -> case cmd of
           1 -> plikProg cmd szyfr
           2 -> plikProg cmd szyfr
           otherwise -> putStrLn "Nie rozpoznano polecenia."
      otherwise -> putStrLn "Nie rozpoznano polecenia."

konsolaProg :: Int -> Int -> IO ()
konsolaProg cmd szyfr = do
      case cmd of
        1 -> putStrLn "Podaj rzecz do zaszyfrowania: "
        2 -> putStrLn "Podaj rzecz do odszyfrowania: "
        _ -> return ()
      line <- getLine
      putStrLn "Podaj klucz: "
      keyIn <- getLine
      putStrLn ("Wczytano: " ++ line)
      putStrLn ("Wczytano klucz: " ++ keyIn)
      case (szyfr, cmd) of
        (1,1) -> konsolaSzyfrCiag line keyIn
        (2,1) -> konsolaSzyfrLista line keyIn
        (3,1) -> konsolaSzyfrCezar line keyIn
        (4,1) -> konsolaSzyfrVig line keyIn
        (1,2) -> konsolaDeszyfrCiag line keyIn
        (2,2) -> konsolaDeszyfrLista line keyIn
        (3,2) -> konsolaDeszyfrCezar line keyIn
        (4,2) -> konsolaDeszyfrVig line keyIn
        otherwise -> putStrLn "Nie rozpoznano polecenia."


konsolaSzyfrCiag :: [Char] -> [Char] -> IO ()
konsolaSzyfrCiag line keyIn = do
      let key = read keyIn :: Int
      putStr "Zaszyfrowany ciąg: "
      Ciag.wyswietlSzyfrowanie line key


konsolaSzyfrLista :: [Char] -> [Char] -> IO ()
konsolaSzyfrLista line keyIn = do
      let key = read keyIn :: [Int]
      let lin = read line :: [Int]
      putStr "Zaszyfrowana lista: "
      wyswietlSzyfrowanieLista lin key

konsolaSzyfrCezar ::  [Char] -> [Char] -> IO ()
konsolaSzyfrCezar line keyIn = do
      let key = read keyIn :: Int
      putStr "Zaszyfrowany ciąg: "
      Cezar.wyswietlSzyfrowanie line key

konsolaSzyfrVig ::  [Char] -> [Char] -> IO ()
konsolaSzyfrVig line keyIn = do
      if (Vigenere.sprawdzCzyDobryKlucz keyIn) then
        do
          putStr "Zaszyfrowany ciąg: "
          Vigenere.wyswietlSzyfrowanie line keyIn
      else
        do
          putStrLn "Błędny klucz :("

konsolaDeszyfrCiag :: [Char] -> [Char] -> IO ()
konsolaDeszyfrCiag line keyIn = do
       if (null keyIn) then
         do
           putStrLn "Mozliwe odszyfrowane ciagu:"
           Ciag.wyswietlPropozycje line
       else
         do
           let key = read keyIn :: Int
           putStr "Odszyfrowany ciąg: "
           Ciag.wyswietlDeszyfrowanie line key

konsolaDeszyfrLista ::  [Char] -> [Char] -> IO ()
konsolaDeszyfrLista line keyIn = do
      let key = read keyIn :: [Int]
      let lin = read line :: [Int]
      putStr "Zaszyfrowana lista: "
      wyswietlDeszyfrowanieLista lin key

konsolaDeszyfrCezar ::  [Char] -> [Char] -> IO ()
konsolaDeszyfrCezar line keyIn = do
      let key = read keyIn :: Int
      putStr "Odszyfrowany ciąg: "
      Cezar.wyswietlDeszyfrowanie line key

konsolaDeszyfrVig ::  [Char] -> [Char] -> IO ()
konsolaDeszyfrVig line keyIn = do
      if (Vigenere.sprawdzCzyDobryKlucz keyIn) then
        do
          putStr "zaszyfrowany ciąg: "
          Vigenere.wyswietlDeszyfrowanie line keyIn
      else
        do
          putStrLn "Błędny klucz :("

plikProg :: Int -> Int -> IO()
plikProg cmd szyfr = do
      putStrLn "Podaj klucz: "
      keyIn <- getLine
      putStrLn ("Wczytano klucz: " ++ keyIn)
      inpStr <- readFile "do_zaszyfrowania.txt"
      case (szyfr, cmd) of
        (1,1) -> plikSzyfrCiag inpStr keyIn
        (2,1) -> putStrLn "Nie wspierane."
        (3,1) -> plikSzyfrCezar inpStr keyIn
        (4,1) -> plikSzyfrVig inpStr keyIn
        (1,2) -> plikDeszyfrCiag inpStr keyIn
        (2,2) -> putStrLn "Nie wspierane."
        (3,2) -> plikDeszyfrCezar inpStr keyIn
        (4,2) -> plikDeszyfrVig inpStr keyIn
        otherwise -> putStrLn "Nie rozpoznano polecenia."

plikSzyfrCiag :: [Char] -> [Char] -> IO ()
plikSzyfrCiag line keyIn = do
        let key = read keyIn :: Int
        writeFile "zakodowane.txt" (Ciag.cSzyfruj line key)
        putStrLn "Zaszyfrowano ciąg."

plikSzyfrCezar ::  [Char] -> [Char] -> IO ()
plikSzyfrCezar line keyIn = do
      let key = read keyIn :: Int
      writeFile "zakodowane.txt" (Cezar.szyfrujCiag line key)
      putStrLn "Zaszyfrowano ciąg."

plikSzyfrVig ::  [Char] -> [Char] -> IO ()
plikSzyfrVig line keyIn = do
      if (Vigenere.sprawdzCzyDobryKlucz keyIn) then
        do
          writeFile "zakodowane.txt" (Vigenere.szyfrujCiag line keyIn)
          putStrLn "Zaszyfrowano ciąg."
      else
        do
          putStrLn "Błędny klucz :("

plikDeszyfrCiag :: [Char] -> [Char] -> IO ()
plikDeszyfrCiag line keyIn = do
      let key = read keyIn :: Int
      writeFile "odkodowane.txt" (Ciag.cDeszyfruj line key)
      putStrLn "Odszyfrowano ciąg."

plikDeszyfrCezar ::  [Char] -> [Char] -> IO ()
plikDeszyfrCezar line keyIn = do
      let key = read keyIn :: Int
      writeFile "odkodowane.txt" (Cezar.deszyfrujCiag line key)
      putStrLn "Odszyfrowano ciąg."

plikDeszyfrVig ::  [Char] -> [Char] -> IO ()
plikDeszyfrVig line keyIn = do
      if (Vigenere.sprawdzCzyDobryKlucz keyIn) then
        do
          writeFile "odkodowane.txt" (Vigenere.deszyfrujCiag line keyIn)
          putStrLn "Zaszyfrowano ciąg."
      else
        do
          putStrLn "Błędny klucz :("
