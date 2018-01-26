{-|
Module      : Vigenere
Description : Moduł odpowiada za szyfrowanie ciągu znaków szyfrem Vigenera
Copyright   : Copyright (c) 2017, Piotr Kedziora
License     : MIT
Maintainer  : albartos.online@gmail.com
Stability   : experimental
Portability : portable

Szyfrowanie z użyciem szyfru Vigenera.
Szyfrowane są duże i małe litery alfabetu łacińskiego,
reszta znaków pozostaje bez zmian
-}

module Vigenere
    (
    -- * Co zawiera moduł
    sprawdzCzyDobryKlucz
    , Vigenere.szyfrujZnak
    , Vigenere.szyfrujCiag
    , Vigenere.wyswietlSzyfrowanie
    , Vigenere.deszyfrujZnak
    , Vigenere.deszyfrujCiag
    , Vigenere.wyswietlDeszyfrowanie
    ) where

import Data.Char
import Cezar
import Alfabet

-- |sprawdza, czy podany klucz jest poprawny,
-- |tzn. czy składa się z samych liter alfabetu łacińskiego (ignorowana jest wielkość liter)
sprawdzCzyDobryKlucz :: [Char] -- ^ sprawdzany klucz szyfrujący
    -> Bool -- ^ czy klucz ma poprawną postać
sprawdzCzyDobryKlucz ks = all (`elem` ['a'..'z']) (map toLower ks)

-- |szyfruje pojedynczy znak
szyfrujZnak :: Char -- ^ szyfrowany znak; znak musi należeć do podawanego zbioru
    -> [Char] -- ^ alfabet w którym szyfrujemy znak
    -> Char -- ^ klucz szyfrujacy postaci litery łacinskiej
    -> Char -- ^ zaszyfrowany znak
szyfrujZnak x xs k = Cezar.szyfrujZnak x xs (pozycjaZnaku k ['a'..'z'])

szyfrujCiag :: [Char] -- ^ ciąg wejściowy
    -> [Char] -- ^ klucz
    -> [Char] -- ^ ciąg wyjściowy
szyfrujCiag xs ks = zipWith (\x k -> if x `elem` ['A'..'Z'] then
                            Vigenere.szyfrujZnak x ['A'..'Z'] k
                          else if x `elem` ['a'..'z'] then
                            Vigenere.szyfrujZnak x ['a'..'z'] k
                          else x) xs (take (length xs) (cycle (map toLower ks)))

-- |wyświetla wynik szyfrowania
wyswietlSzyfrowanie :: [Char] -- ^ ciąg wejściowy
    -> [Char] -- ^ klucz szyfrujący
    -> IO () -- ^ IO
wyswietlSzyfrowanie xs ks = print (Vigenere.szyfrujCiag xs ks)

deszyfrujZnak :: Char
    -> [Char]
    -> Char
    -> Char
deszyfrujZnak x xs k = Cezar.deszyfrujZnak x xs (pozycjaZnaku k ['a'..'z'])

-- |deszyfruje ciąg znaków
deszyfrujCiag :: [Char] -- ^ zaszyfrowany ciąg
    -> [Char] -- ^ klucz szyfrujący
    -> [Char] -- ^ oryginalny ciąg
deszyfrujCiag xs ks = zipWith (\x k -> if x `elem` ['A'..'Z'] then
                            Vigenere.deszyfrujZnak x ['A'..'Z'] k
                          else if x `elem` ['a'..'z'] then
                            Vigenere.deszyfrujZnak x ['a'..'z'] k
                          else x) xs (take (length xs) (cycle (map toLower ks)))

-- |wyświetla wynik deszyfrowania
wyswietlDeszyfrowanie :: [Char] -- ^ ciąg zaszyfrowany
    -> [Char] -- ^ klucz szyfrujący
    -> IO () -- ^ IO
wyswietlDeszyfrowanie xs ks = print (Vigenere.deszyfrujCiag xs ks)
