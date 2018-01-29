{-# LANGUAGE TemplateHaskell #-}
module QuickCheckTests where

import Test.QuickCheck
import Test.QuickCheck.All
import Data.Char

import Ciag;
import Lista;
import Cezar;
import Vigenere;

newtype AllowedLatinChar = AllowedLatinChar Char deriving (Eq, Show)
newtype AllowedKeyChar = AllowedKeyChar Char deriving (Eq, Show)

instance Arbitrary AllowedLatinChar where
  arbitrary = fmap AllowedLatinChar (elements (['A'..'Z'] ++ ['a' .. 'z'] ++ " ~!@#$%^&*()"))

instance Arbitrary AllowedKeyChar where
  arbitrary = fmap AllowedKeyChar (elements (['A'..'Z'] ++ ['a' .. 'z']))

prop_DecEncSeq :: [Char] -> NonNegative Int -> Bool
prop_DecEncSeq a (NonNegative key) =
  Ciag.cDeszyfruj (Ciag.cSzyfruj a key ) key == a

prop_EncDecSeq :: [Char] -> NonNegative Int -> Bool
prop_EncDecSeq a (NonNegative key) =
  Ciag.cSzyfruj (Ciag.cDeszyfruj a key) key == a

prop_DecEncList :: (NonEmptyList Integer) -> (NonEmptyList Integer) -> Bool
prop_DecEncList (NonEmpty a) (NonEmpty key) =
  Lista.lDeszyfruj (Lista.lSzyfruj a key) key == a

prop_EncDecList :: (NonEmptyList Integer) -> (NonEmptyList Integer) -> Bool
prop_EncDecList (NonEmpty a) (NonEmpty key) =
  Lista.lSzyfruj (Lista.lDeszyfruj a key) key == a

prop_DecEncCesarSeq :: [AllowedLatinChar] -> Int -> Bool
prop_DecEncCesarSeq aa key =
  Cezar.deszyfrujCiag (Cezar.szyfrujCiag a key) key == a where
  a = [x | (AllowedLatinChar x) <- aa]

prop_EncDecCesarSeq :: [AllowedLatinChar] -> Int -> Bool
prop_EncDecCesarSeq aa key =
  Cezar.szyfrujCiag (Cezar.deszyfrujCiag a key) key == a where
  a = [x | (AllowedLatinChar x) <- aa]

prop_DecEncVigSeq :: [AllowedLatinChar] -> (NonEmptyList AllowedKeyChar) -> Bool
prop_DecEncVigSeq aa (NonEmpty kk) =
  Vigenere.szyfrujCiag (Vigenere.deszyfrujCiag a key) key == a where
  a = [x | (AllowedLatinChar x) <- aa]
  key = [x | (AllowedKeyChar x) <- kk]

prop_EncDecVigSeq :: [AllowedLatinChar] -> (NonEmptyList AllowedKeyChar) -> Bool
prop_EncDecVigSeq aa (NonEmpty kk) =
  Vigenere.deszyfrujCiag (Vigenere.szyfrujCiag a key) key == a where
  a = [x | (AllowedLatinChar x) <- aa]
  key = [x | (AllowedKeyChar x) <- kk]

return []
runTests :: IO Bool
runTests = $(quickCheckAll)
