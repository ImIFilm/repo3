import Data.Char

-- | To jest nasza glowna funkcja
encoding = do
    txt <- getLine
    putStr "Wczytano: "
    putStrLn (txt)
    putStrLn "Podaj liczbe kodujaca: "
    liczbaKodujaca <- getLine
    putStr "Wczytano: "
    putStrLn (liczbaKodujaca)


    let num = read liczbaKodujaca :: Int
    let length1 = length txt
    putStr "Dlugosc naszego ciagu: "
    print length1
    let lista = map (\x -> [x]) txt --liata cyfr
    putStr "Tak wyglada nasza lista: "
    print lista

    let lista2 = fmap(\x -> read x::Int)lista
    putStr "Tak wyglada nasza lista jako lista Intów: "
    print lista2

    let lista3 = fmap(\x -> (x + num) `mod`  10)lista2
    putStr "Tak wyglada nasza lista Intów po zakodowaniu: "
    print lista3
