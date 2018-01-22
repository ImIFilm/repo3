{-|
Module      : Ciag
Description : Moduł odpowiada za szyfrowanie
Copyright   : Copyright (c) 2017, Piotr Kedziora
License     : MIT
Maintainer  : imipulawy@gmail.com
Stability   : experimental
Portability : portable

cala magia tkwi w funkcjach @cSzyfruj@ i @cDeszyfruj@
-}

module Ciag
    (
     -- * Co zawiera nasz moduł?
    cSzyfruj
    , wyswietlSzyfrowanie
    , cDeszyfruj
    , wyswietlDeszyfrowanie
    , cProbuj
    , wyswietlPropozycje
     ) where

import Data.Char        

-- |szyfruje ciag znaków
cSzyfruj :: Functor f =>
    f Char -- ^ ciąg wejsciowy
    -> Int -- ^ klucz
    -> f Char -- ^ ciąg wyjsciowy
cSzyfruj a b = fmap (\x->chr ((ord x)+b)) a    

-- |wyswietla nam wynik naszego szyfrowania
wyswietlSzyfrowanie :: (Functor f, Show (f Char)) => f Char -- ^ ciąg wejsciowy
    -> Int -- ^ klucz szyfrujący
    -> IO () -- ^ IO
wyswietlSzyfrowanie a b = print (cSzyfruj a b)

-- |deszyfruje ciąg znaków
cDeszyfruj :: Functor f => f Char -- ^ zaszyfrowany ciąg
    -> Int -- ^ klucz szyfrujący
    -> f Char -- ^ oryginalny ciąg
cDeszyfruj a b = fmap (\x->chr ((ord x)-(b `mod` 30))) a

-- |wyswietla nam wynik naszego deszyfrowania
wyswietlDeszyfrowanie :: (Functor f, Show (f Char)) => f Char -- ^ ciąg zaszyfrowany
    -> Int -- ^ klucz szyfrujący
    -> IO () -- ^ IO
wyswietlDeszyfrowanie a b = print (cDeszyfruj a b)

-- |próbuje nam pomoc w rozszyfrowaniu
cProbuj :: [Char] -- ^ ciag zaszyfrowany
    -> [Char] -- ^ różne kombinacje
cProbuj a = [j | i <- [1..29], j<-(cDeszyfruj a i)]

-- |wyswietla nam pomoc w wyszukaniu oryginalu
wyswietlPropozycje :: [Char] -- ^ ciąg zaszyfrowany
    -> IO () -- ^ IO
wyswietlPropozycje a = print (cProbuj a)