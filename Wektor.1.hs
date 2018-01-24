{-|
Module      : Lista
Description : Moduł odpowiada za szyfrowanie wektorow
Copyright   : Copyright (c) 2017, Piotr Kedziora
License     : MIT
Maintainer  : imipulawy@gmail.com
Stability   : experimental
Portability : portable

w zasadzie to chodzi o to, że jak mamy wetkor [1, 2, 3 to szyfrujemy go dodając do każdego elementu jego length
-}

module Wektor
    (
     -- * Co zawiera nasz moduł?
    wSzyfruj
    , wyswietlSzyfrowanieWektor
    , wDeszyfruj
    --, wyswietlZdeszyfrowanieWektor
     ) where

import Data.Vector as V

-- | szyfruje nasz wetkor
wSzyfruj x = Data.Vector.map ((+)(Data.Vector.length x)) x

-- | wyswietla zaszyfrowany wektor
wyswietlSzyfrowanieWektor x = print (wSzyfruj x)

-- | deszyfruje nasz wektor
wDeszyfruj x = Data.Vector.map ((+)(-Data.Vector.length x)) x

-- | wyswietla zdeszyfrowany wektor
wyswietlZdeszyfrowanieWektor x = print (wDeszyfruj x)