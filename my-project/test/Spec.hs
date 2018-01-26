import Test.HUnit
import Cezar
import Vigenere

tests = TestList [
    testSzyfrZnakCezar1,
    testSzyfrZnakCezar2,
    testDeszyfrZnakCezar1,
    testDeszyfrZnakCezar2,
    testSzyfrCiagCezar,
    testDeszyfrCiagCezar,
    testSzyfrZnakVig,
    testDeszyfrZnakVig,
    testSzyfrCiagVig,
    testDeszyfrCiagVig
    ]

main :: IO Counts
main = runTestTT tests

testSzyfrZnakCezar1 :: Test
testSzyfrZnakCezar1 =
    TestCase $ assertEqual
              "Szyfrowanie znaku szyfrem Cezara nie działa poprawnie"
              'U'
              (Cezar.szyfrujZnak 'D' ['A'..'Z'] 17)

testSzyfrZnakCezar2 :: Test
testSzyfrZnakCezar2 =
    TestCase $ assertEqual
              "Szyfrowanie znaku szyfrem Cezara nie działa poprawnie"
              'e'
              (Cezar.szyfrujZnak 'v' ['a'..'Z'] 139)

testDeszyfrZnakCezar1 :: Test
testDeszyfrZnakCezar1 =
    TestCase $ assertEqual
              "Deszyfrowanie znaku w szyfrze Cezara nie działa poprawnie"
              'D'
              (Cezar.deszyfrujZnak 'U' ['A'..'Z'] 17)

testDeszyfrZnakCezar2 :: Test
testDeszyfrZnakCezar2 =
    TestCase $ assertEqual
              "Deszyfrowanie znaku w szyfrze Cezara nie działa poprawnie"
              'v'
              (Cezar.deszyfrujZnak 'e' ['a'..'z'] 139)

testSzyfrCiagCezar :: Test
testSzyfrCiagCezar =
    TestCase $ assertEqual
              "Szyfrowanie Cezara nie działa poprawnie"
              "Xufia, Avolklza yavm!"
              (Cezar.szyfrujCiag "Litwo, Ojczyzno moja!" 12)

testSzyfrZnakVig :: Test
testSzyfrZnakVig =
    TestCase $ assertEqual
              "Szyfrowanie znaku szyfrem Vigenere nie działa poprawnie"
              'U'
              (Vigenere.szyfrujZnak 'F' ['A'..'Z'] 'p')

testDeszyfrZnakVig :: Test
testDeszyfrZnakVig =
    TestCase $ assertEqual
              "Deszyfrowanie znaku w szyfrze Cezara nie działa poprawnie"
              'l'
              (Vigenere.deszyfrujZnak 'e' ['a'..'z'] 't')

testDeszyfrCiagCezar :: Test
testDeszyfrCiagCezar =
    TestCase $ assertEqual
              "Dezyfrowanie Cezara nie działa poprawnie"
              "A jego liczba czterdziesci i cztery"
              (Cezar.deszyfrujCiag "S bwyg daurts urlwjvrawkua a urlwjq" 44)

testSzyfrCiagVig :: Test
testSzyfrCiagVig  =
    TestCase $ assertEqual
              "Szyfrowanie Vigenera nie działa poprawnie"
              "Eis wcyoz jjby h xyjis mglkfi!"
              (Vigenere.szyfrujCiag "You fight like a dairy farmer!" "Guybrush")

testDeszyfrCiagVig :: Test
testDeszyfrCiagVig =
    TestCase $ assertEqual
              "Deszyfrowanie Vigenera nie działa poprawnie"
              "Jeden wie, wszyscy moga wiedziec!"
              (Vigenere.deszyfrujCiag "Fhlaq slm, eocgofg pwcd slmzcqaf!" "wdi")
