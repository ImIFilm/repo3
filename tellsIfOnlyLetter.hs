import Data.Char

tellsIfOnlyLetter [] = True
tellsIfOnlyLetter (x:xs) = 
    if isLetter x then tellsIfOnlyLetter (xs)
    else False