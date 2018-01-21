module Ciag
    {  Ciag (..)
    , cSzyfruj
    , cDeszyfruj
    } where

data Ciag = Ciag
        { kod :: String
        , przesuniecie :: Int
        } deriving (Eq, Show)

cSzyfruj :: Functor f => f Char -> Int -> f Char
cSzyfruj a b = fmap (\x->chr ((ord x)+b)) a    

cDeszyfruj :: Functor f => f Char -> Int -> f Char
cDeszyfruj a b = fmap (\x->chr ((ord x)-(b `mod` 30))) a