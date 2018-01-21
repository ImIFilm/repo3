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
    putStr "Dlugosc naszego ciagu: "
    print length1

    let g = fmap (\x->ord x) txt
    print g
    let g1 = fmap (\x-> x + num) g 
    print g1
    let g2 = fmap (\x->chr x) g1
    print g2
    

tellsIfOnlyLetter [] = True
tellsIfOnlyLetter (x:xs) = 
    if isLetter x then tellsIfOnlyLetter (xs)
    else False