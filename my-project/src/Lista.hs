{-|
Module      : Lista
Description : Moduł odpowiada za szyfrowanie list
Copyright   : Copyright (c) 2017, Piotr Kedziora
License     : MIT
Maintainer  : imipulawy@gmail.com
Stability   : experimental
Portability : portable

cala magia tkwi w funkcjach @lSzyfruj@ i @lDeszyfruj@
-}

module Lista
    (
     -- * Co zawiera nasz moduł?
    lSzyfruj
    , wyswietlSzyfrowanieLista
    , lDeszyfruj
    , wyswietlDeszyfrowanieLista
     ) where

import Data.Char        

-- |szyfruje ciag cyfr
lSzyfruj :: Num a => [a] -- ^ lista na wejsciu
    -> [a] -- ^ klucz
    -> [a] -- ^ wynik
lSzyfruj x y = zipWith (+) x (take (length x) (cycle y))

-- |wyswietla nam wynik naszego szyfrowania
wyswietlSzyfrowanieLista :: (Num a, Show a) => [a] -- ^ lista na wejsciu
    -> [a] -- ^ klucz
    -> IO () -- ^ IO
wyswietlSzyfrowanieLista a b = print (lSzyfruj a b)

-- |deszyfruje ciag cyfr
lDeszyfruj :: Num a => [a] -- ^ lista zaszyfrowana
    -> [a] -- ^ klucz
    -> [a] -- ^ lista oryginalna
lDeszyfruj x y = zipWith (-) x (take (length x) (cycle y))

-- |wyswietla wynik deszyfracji listy
wyswietlDeszyfrowanieLista :: (Num a, Show a) => [a] -- ^ lista zaszyfrowana
    -> [a] -- ^ klucz
    -> IO () -- ^ IO
wyswietlDeszyfrowanieLista a b = print (lDeszyfruj a b)