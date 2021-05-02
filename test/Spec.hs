import Data.Monoid
import Control.Monad
-- import Utils
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Lib

sut :: Enigma
sut = createEnigma ["BDFHJLCPRTXVZNYEIWGAKMUSQO","AJDKSIRUXBLHWTMCQGZNPYFVOE","EKMFLGDQVZNTOWYHXUSPAIBRCJ"] "YRUHQSLDPXNGOKMIEBFZCWVJAT"

enigmaAllRotor1 :: Enigma
enigmaAllRotor1 = createEnigma ["EKMFLGDQVZNTOWYHXUSPAIBRCJ","EKMFLGDQVZNTOWYHXUSPAIBRCJ","EKMFLGDQVZNTOWYHXUSPAIBRCJ"] "YRUHQSLDPXNGOKMIEBFZCWVJAT"

genAlphabetChar :: Gen Char
genAlphabetChar = elements ['A'..'Z']

genAlphabetString :: Gen String
genAlphabetString = listOf genAlphabetChar

newtype AlphabetString = AlphabetString { unwrapAlphabetString :: String} deriving Show

instance Arbitrary AlphabetString where
  arbitrary = AlphabetString <$> genAlphabetString

singleCharacterTest :: Assertion
singleCharacterTest = do
  let c = "A"
  let b = singleCharacterProperty c
  assertBool "Encoded character should decode to original starting character" b

singleCharacterProperty :: String -> Bool
singleCharacterProperty c = do
  let a = encode sut c
  let b = encode sut a
  c == b

singleCharacterNeverEncodesToItselfProperty :: String -> Bool
singleCharacterNeverEncodesToItselfProperty c = do
  let a = encode sut c
  c /= a

encodedStringShouldNotBeOriginalStringProperty :: String -> Bool
encodedStringShouldNotBeOriginalStringProperty s = do
  let a = encode sut s
  s /= a

encodedStringShouldDecodeToItselfProperty :: String -> Bool
encodedStringShouldDecodeToItselfProperty s = do
  let a = encode sut s
  let b = encode sut a
  s == b

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

unitTests = testGroup "Unit tests"
  [
    testCase "singleCharacterTest" singleCharacterTest
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [
      QC.testProperty "singleCharacterTest" $ QC.forAll (elements ['A'..'Z']) $ \c -> singleCharacterProperty [c]
    , QC.testProperty "singleCharacterNeverEncodesToItselfProperty" $ QC.forAll (elements ['A'..'Z']) $ \c -> singleCharacterNeverEncodesToItselfProperty [c]
    , QC.testProperty "encodedStringShouldNotBeOriginalStringProperty" $ QC.forAll genAlphabetString encodedStringShouldNotBeOriginalStringProperty
    , QC.testProperty "encodedStringShouldDecodeToItselfProperty" $ QC.forAll genAlphabetString encodedStringShouldDecodeToItselfProperty
  ]
