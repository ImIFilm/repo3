{-|
Module      : Alfabet
Description : Pomocniczy moduł, zawierający funkcję operujące na alfabecie
Copyright   : Copyright (c) 2017, Bartłomiej Stępień
License     : MIT
Maintainer  : albartos.online@gmail.com
Stability   : experimental
Portability : portable

-}

module Alfabet
    (
    pozycjaZnaku
    ) where

import Data.List
import Data.Maybe

-- pomocnicza funkcja, określająca pozycję znaku w alfabecie
pozycjaZnaku :: Char -- szukany znak
  -> [Char] -- alfabet, w którym szukamy znaku (znak musi należeć do alfabetu)
  -> Int -- pozycja znaku w alfabecie
pozycjaZnaku x xs = fromJust (elemIndex x xs)
