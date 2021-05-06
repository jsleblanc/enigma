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

enigmaAllRotor1_StartFirstRotorAtZ :: Enigma
enigmaAllRotor1_StartFirstRotorAtZ = createEnigmaWithRotors [r3, r2, r1] rfl
  where
    r1 = createRotor "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 0
    r2 = createRotor "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 0
    r3 = createRotor "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 25
    rfl = createRotor "YRUHQSLDPXNGOKMIEBFZCWVJAT" 0

genAlphabetChar :: Gen Char
genAlphabetChar = elements ['A'..'Z']

genAlphabetString :: Gen String
genAlphabetString = listOf1 genAlphabetChar

newtype AlphabetChar = AlphabetChar { unwrapAlphabetChar :: Char } deriving Show
newtype AlphabetString = AlphabetString { unwrapAlphabetString :: String } deriving Show

instance Arbitrary AlphabetChar where
  arbitrary = AlphabetChar <$> genAlphabetChar

instance Arbitrary AlphabetString where
  arbitrary = AlphabetString <$> genAlphabetString

cipheredExample_1_Test :: Assertion
cipheredExample_1_Test = do
  let sut = enigmaAllRotor1
  let result = evalState (encode "AAAAAAAAAAAAAAA") sut
  assertEqual "Example did not encode to expected value" "UOTGRLFGERCPELT" result

cipheredExample_2_Test :: Assertion
cipheredExample_2_Test = do
  let sut = enigmaAllRotor1_StartFirstRotorAtZ
  let result = evalState (encode "AAAAAAAAAAAAAAA") sut
  assertEqual "Example did not encode to expected value" "JUOTGRLFGERCPEL" result


singleCharacterNeverEncodesToItselfProperty :: AlphabetChar -> Property
singleCharacterNeverEncodesToItselfProperty (AlphabetChar c) = True ==> do
  let a = evalState (encode [c]) sut
  c /= head a

encodedStringShouldNotBeOriginalStringProperty :: AlphabetString -> Property
encodedStringShouldNotBeOriginalStringProperty (AlphabetString s) = True ==> do
  let a = evalState (encode s) sut
  s /= a

encodedStringShouldDecodeToItselfProperty :: AlphabetString -> Property
encodedStringShouldDecodeToItselfProperty (AlphabetString s) = True ==> do
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
    , testCase "cipheredExample_2_Test" cipheredExample_2_Test
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [
      QC.testProperty "singleCharacterNeverEncodesToItselfProperty" singleCharacterNeverEncodesToItselfProperty
    , QC.testProperty "encodedStringShouldNotBeOriginalStringProperty" encodedStringShouldNotBeOriginalStringProperty
    , QC.testProperty "encodedStringShouldDecodeToItselfProperty" encodedStringShouldDecodeToItselfProperty
  ]
