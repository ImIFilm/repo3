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
lSzyfruj x y = zipWith (+) x (take (length x) (cycle y))

-- |wyswietla nam wynik naszego szyfrowania
wyswietlSzyfrowanieLista a b = print (lSzyfruj a b)

-- |deszyfruje ciag cyfr
lDeszyfruj x y = zipWith (-) x (take (length x) (cycle y))

-- |wyswietla wynik deszyfracji listy
wyswietlDeszyfrowanieLista a b = print (lDeszyfruj a b)

{-

-- |wyswietla nam wynik naszego szyfrowania
wyswietlSzyfrowanieLista :: (Functor f, Show (f Char)) => f Char -- ^ ciąg wejsciowy
    -> Int -- ^ klucz szyfrujący
    -> IO () -- ^ IO
wyswietlSzyfrowanieLista a b = print (cSzyfruj a b)

-- |deszyfruje ciąg znaków
lDeszyfruj :: Functor f => f Char -- ^ zaszyfrowany ciąg
    -> Int -- ^ klucz szyfrujący
    -> f Char -- ^ oryginalny ciąg
lDeszyfruj a b = fmap (\x->chr ((ord x)-(b `mod` 30))) a

-- |wyswietla nam wynik naszego deszyfrowania
wyswietlDeszyfrowanieLista :: (Functor f, Show (f Char)) => f Char -- ^ ciąg zaszyfrowany
    -> Int -- ^ klucz szyfrujący
    -> IO () -- ^ IO
wyswietlDeszyfrowanieLista a b = print (cDeszyfruj a b)

-}