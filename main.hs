import Data.Char

-- | To jest nasza glowna funkcja
encoding = do
    txt <- getLine
    putStr "Wczytano: "
    putStrLn (txt)
    putStrLn "Podaj liczbe kodujaca: "
    liczbaKodujaca <- getLine
    putStr "Wczytano liczbe kodujaca: "
    putStrLn (liczbaKodujaca)
    let num = read liczbaKodujaca :: Int
    let length1 = length txt
    putStr "__Dlugosc naszego ciagu: "
    print length1

    let g = cSzyfruj txt num
    print g

cSzyfruj a b = fmap (\x->chr ((ord x)+(b `mod` 30))) a

cDeszyfruj a b = fmap (\x->chr ((ord x)-(b `mod` 30))) a

tellsIfOnlyLetter [] = True
tellsIfOnlyLetter (x:xs) = 
    if isLetter x then tellsIfOnlyLetter (xs)
    else False