module Lib
    ( someFunc, cSzyfruj
    ) where
 
import Data.Char        

--someFunc :: [Char] -> Integer -> IO ()
someFunc = print (cSzyfruj "asd" 1)

cSzyfruj :: Functor f => f Char -> Int -> f Char
cSzyfruj a b = fmap (\x->chr ((ord x)+b)) a
