import Test.HUnit
import Cezar
import Vigenere

tests = TestList [
    testSzyfrZnakCezar,
    testSzyfrCiagCezar,
    testSzyfrZnakVig,
    testSzyfrCiagVig
    ]

main :: IO Counts
main = runTestTT tests

testSzyfrZnakCezar :: Test
testSzyfrZnakCezar =
    TestCase $ assertEqual
              "Szyfrowanie znaku szyfrem Cezara nie działa poprawnie"
              'U'
              (Cezar.szyfrujZnak 'D' ['A'..'Z'] 17)

testSzyfrCiagCezar :: Test
testSzyfrCiagCezar =
    TestCase $ assertEqual
              "Szyfrowanie Cezara nie dziala poprawnie"
              "Xufia, Avolklza yavm!"
              (Cezar.szyfrujCiag "Litwo, Ojczyzno moja!" 12)

testSzyfrZnakVig :: Test
testSzyfrZnakVig =
    TestCase $ assertEqual
              "Szyfrowanie znaku szyfrem Vigenere nie działa poprawnie"
              'U'
              (Vigenere.szyfrujZnak 'F' ['A'..'Z'] 'p')

testSzyfrCiagVig :: Test
testSzyfrCiagVig  =
    TestCase $ assertEqual
              "Szyfrowanie Vigenera nie działa poprawnie"
              "Eis wcyoz jjby h xyjis mglkfi!"
              (Vigenere.szyfrujCiag "You fight like a dairy farmer!" "Guybrush")
