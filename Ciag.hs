module Ciag
    {  Ciag (..)
    , cSzyfruj
    , cDeszyfruj
    } where

data Ciag = Ciag
        { kod :: String
        , przesuniecie :: Int
        } deriving (Eq, Show)

cSzyfruj :: Ciag -> Ciag -> Ciag
cSzyfruj (Ciag a b) = do let h = fmap (\x->fromEnum x)
                        let h = fmap (\x->x+przesuniecie)
                        let h = fmap (\x->toEnum x)
                        Ciag (h, b)                        