import Data.Char

-- | To jest nasza glowna funkcja
encoding = do
    txt <- getLine
    putStr "Wczytano: "
    putStrLn (txt)
    let num = read txt :: Int
    let length1 = length txt
    putStr "Dlugosc naszego ciagu: "
    print length1
    let lista = map (\x -> [x]) txt --liata cyfr
    putStr "Tak wyglada nasza lista: "
    print lista

