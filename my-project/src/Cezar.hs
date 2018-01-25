{-|
Module      : Cezar
Description : Moduł odpowiada za szyfrowanie ciągu znaków szyfrem Cezara
Copyright   : Copyright (c) 2017, Bartłomiej Stępień
License     : MIT
Maintainer  : albartos.online@gmail.com
Stability   : experimental
Portability : portable

Szyfrowanie z użyciem szyfru Cezara.
Szyfrowane są duże i małe litery alfabetu łacińskiego, reszta pozostaje bez zmian
-}

module Cezar
    (
    -- * Co zawiera nasz moduł?
    szyfruj
    , wyswietlSzyfrowanie
    , deszyfruj
    , wyswietlDeszyfrowanie
    ) where


import Data.List
import Data.Maybe

-- |szyfruje ciąg znaków
szyfruj :: Functor f =>
    f Char -- ^ ciąg wejściowy
    -> Int -- ^ klucz szyfrujący
    -> f Char -- ^ ciag wyjściowy
szyfruj a b = fmap (\x -> if x `elem` ['A'..'Z'] then
                            szyfrujZnak x ['A'..'Z'] b
                          else if x `elem` ['a'..'z'] then
                            szyfrujZnak x ['a'..'z'] b
                          else x) a

-- pomocnicza funkjca, szyfrujaca pojedyczny znak;
-- znak musi należeć do podawanego zbioru
szyfrujZnak :: Char -> [Char] -> Int -> Char
szyfrujZnak x xs k = xs !! ((fromJust (elemIndex x xs) + k) `mod` (length xs))

-- |wyświetla wynik szyfrowania
wyswietlSzyfrowanie :: (Functor f, Show (f Char)) =>
    f Char -- ^ ciąg wejściowy
    -> Int -- ^ klucz szyfrujący
    -> IO () -- ^ IO
wyswietlSzyfrowanie a b = print (szyfruj a b)

-- |deszyfruje ciąg znaków
deszyfruj :: Functor f =>
    f Char -- ^ zaszyfrowany ciąg
    -> Int -- ^ klucz szyfrujący
    -> f Char -- ^ oryginalny ciąg
deszyfruj a b = fmap (\x -> if x `elem` ['A'..'Z'] then
                            deszyfrujZnak x ['A'..'Z'] b
                          else if x `elem` ['a'..'z'] then
                            deszyfrujZnak x ['a'..'z'] b
                          else x) a

-- pomocnicza funkjca, szyfrujaca pojedyczny znak;
-- znak musi należeć do podawanego zbioru
deszyfrujZnak :: Char -> [Char] -> Int -> Char
deszyfrujZnak x xs k =
  xs !! ((fromJust (elemIndex x xs) - k + (length xs)) `mod` (length xs))

-- |wyświetla wynik deszyfrowania
wyswietlDeszyfrowanie :: (Functor f, Show (f Char)) =>
    f Char -- ^ ciąg zaszyfrowany
    -> Int -- ^ klucz szyfrujący
    -> IO () -- ^ IO
wyswietlDeszyfrowanie a b = print (deszyfruj a b)
