{-|
Module      : Lib
Description : Pierwszy modul
Copyright   : Copyright (c) 2017, Piotr Kedziora
License     : MIT
Maintainer  : imipulawy@gmail.com
Stability   : experimental
Portability : portable

a tu troche wiecej powinno byc napisane o tym module, ale nic wiecej nie napisze, tylko pokaze to @some markup@
-}

module Lib
    ( 
    -- * Some functions    
    someFunc, cSzyfruj2
    ) where
 
import Data.Char        

-- * To jest naglowek

-- |wyswietla nam wynik naszego szyfrowania
someFunc :: IO() -- ^ wejscie/wyjscie
someFunc = print (cSzyfruj2 "asd" 1)

cSzyfruj2 :: Functor f => f Char -- ^ ciag wejsciowy
            -> Int -- ^ liczba szyfrujaca
            -> f Char -- ^ ciag znakow
cSzyfruj2 a b = fmap (\x->chr ((ord x)+b)) a
