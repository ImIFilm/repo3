{-|
Module      : Cezar
Description : Szyfrowanie z użyciem szyfru Cezara.
Copyright   : Copyright (c) 2017, Bartłomiej Stępień
License     : MIT
Maintainer  : albartos.online@gmail.com
Stability   : experimental
Portability : portable

Moduł odpowiada za szyfrowanie ciągu znaków szyfrem Cezara.
Szyfrowane są duże i małe litery alfabetu łacińskiego,
reszta znaków pozostaje bez zmian
-}

module Cezar
    (
    -- * Co zawiera moduł
    Cezar.szyfrujZnak
    , Cezar.szyfrujCiag
    , Cezar.wyswietlSzyfrowanie
    , Cezar.deszyfrujZnak
    , Cezar.deszyfrujCiag
    , Cezar.wyswietlDeszyfrowanie
    ) where

import Alfabet

-- |szyfruje pojedyczny znak
szyfrujZnak :: Char -- ^ szyfrowany znak; znak musi należeć do podawanego zbioru
    -> [Char] -- ^ alfabet w ktorym szyfrujemy znak
    -> Int -- ^ klucz szyfrujący
    -> Char -- zaszyfrowany znak
szyfrujZnak x xs k = xs !! ((pozycjaZnaku x xs + k) `mod` (length xs))

-- |szyfruje ciąg znaków
szyfrujCiag :: [Char] -- ^ ciąg wejściowy
    -> Int -- ^ klucz szyfrujący
    -> [Char] -- ^ ciag wyjściowy
szyfrujCiag xs k = map (\x -> if x `elem` ['A'..'Z'] then
                            szyfrujZnak x ['A'..'Z'] k
                          else if x `elem` ['a'..'z'] then
                            szyfrujZnak x ['a'..'z'] k
                          else x) xs

-- |wyświetla wynik szyfrowania
wyswietlSzyfrowanie :: [Char] -- ^ ciąg wejściowy
    -> Int -- ^ klucz szyfrujący
    -> IO () -- ^ IO
wyswietlSzyfrowanie xs k = print (szyfrujCiag xs k)

-- |deszyfruje pojedyczny znak
deszyfrujZnak :: Char
    -> [Char] -- ^ alfabet w którym zaszyfrowany był znak
    -> Int -- ^ klucz szyfrujący postaci litery łacińskiej
    -> Char -- ^ odszyfrowany znak
deszyfrujZnak x xs k =
  xs !! ((pozycjaZnaku x xs - k + (length xs)) `mod` (length xs))

-- |deszyfruje ciąg znaków
deszyfrujCiag :: [Char] -- ^ zaszyfrowany ciąg
    -> Int -- ^ klucz szyfrujący
    -> [Char] -- ^ oryginalny ciąg
deszyfrujCiag xs k = map (\x -> if x `elem` ['A'..'Z'] then
                            deszyfrujZnak x ['A'..'Z'] k
                          else if x `elem` ['a'..'z'] then
                            deszyfrujZnak x ['a'..'z'] k
                          else x) xs

-- |wyświetla wynik deszyfrowania
wyswietlDeszyfrowanie :: [Char] -- ^ ciąg zaszyfrowany
    -> Int -- ^ klucz szyfrujący
    -> IO () -- ^ IO
wyswietlDeszyfrowanie a b = print (deszyfrujCiag a b)
