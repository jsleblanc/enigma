import Data.Monoid
import Control.Monad.State
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

cipheredExample_1_Test :: Assertion
cipheredExample_1_Test = do
  let result = evalState (encode "AAAA") enigmaAllRotor1
  assertEqual "Example did not encode to expected value" result "UOTG"




singleCharacterNeverEncodesToItselfProperty :: Char -> Bool
singleCharacterNeverEncodesToItselfProperty c = do
  let a = evalState (encode [c]) sut
  c /= head a

encodedStringShouldNotBeOriginalStringProperty :: String -> Bool
encodedStringShouldNotBeOriginalStringProperty s = do
  let a = evalState (encode s) sut
  s /= a

encodedStringShouldDecodeToItselfProperty :: String -> Bool
encodedStringShouldDecodeToItselfProperty s = do
  let a = evalState (encode s) sut
  let b = evalState (encode a) sut
  s == b


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

unitTests = testGroup "Unit tests"
  [
    testCase "cipheredExample_1_Test" cipheredExample_1_Test
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [
    QC.testProperty "singleCharacterNeverEncodesToItselfProperty" $ QC.forAll genAlphabetChar singleCharacterNeverEncodesToItselfProperty
    , QC.testProperty "encodedStringShouldNotBeOriginalStringProperty" $ QC.forAll genAlphabetString encodedStringShouldNotBeOriginalStringProperty
    , QC.testProperty "encodedStringShouldDecodeToItselfProperty" $ QC.forAll genAlphabetString encodedStringShouldDecodeToItselfProperty
  ]
