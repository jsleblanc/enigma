import Control.Monad.State
-- import Utils
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Lib

enigmaAllRotor1 :: Enigma
enigmaAllRotor1 = 
  createEnigmaWithRotors [r3, r2, r1] reflectorB emptyPlugboard
    where
      r1 = rotorI 0
      r2 = rotorI 0
      r3 = rotorI 0

enigmaAllRotor1_StartFirstRotorAtZ :: Enigma
enigmaAllRotor1_StartFirstRotorAtZ = 
  createEnigmaWithRotors [r3, r2, r1] reflectorB emptyPlugboard
    where
      r1 = rotorI 0
      r2 = rotorI 0
      r3 = rotorI 25

enigmaConfig2 :: Enigma
enigmaConfig2 = 
  createEnigmaWithRotors [r3, r2, r1] reflectorB emptyPlugboard
    where
      r1 = rotorIII 0
      r2 = rotorII 0
      r3 = rotorI 0
      
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

cipheredExample_3_Test :: Assertion
cipheredExample_3_Test = do
  let sut = enigmaAllRotor1
  let plainText = replicate 26 'A'
  let result = evalState (encode plainText) sut
  assertEqual "Example did not encode to expected value. 2nd rotor should have rotated once." "UOTGRLFGERCPELTTVTJBGHVWPY" result

cipheredExample_4_Test :: Assertion
cipheredExample_4_Test = do
  let sut = enigmaAllRotor1
  let plainText = replicate 52 'A'
  let result = evalState (encode plainText) sut
  assertEqual "Example did not encode to expected value. 2nd rotor should have rotated once." "UOTGRLFGERCPELTTVTJBGHVWPYEUMJYTVIZFNVFWYFJVOBTTNQRU" result

cipheredExample_5_Test :: Assertion
cipheredExample_5_Test = do
  let sut = enigmaConfig2
  let plainText = replicate 52 'A'
  let result = evalState (encode plainText) sut
  assertEqual "Example did not encode to expected value. 2nd rotor should have rotated once." "FTZMGISXIPJWGDNJJCOQTYRIGDMXFIESRWZGTOIUIEKKDCSHTPYO" result

cipheredExampleDoubleStepping :: Assertion
cipheredExampleDoubleStepping = do
  let r1 = rotorIII 10
  let r2 = rotorII 3
  let r3 = rotorI 14
  let sut = createEnigmaWithRotors [r3, r2, r1] reflectorB emptyPlugboard
  let plainText = replicate 9 'A'
  let result = evalState (encode plainText) sut
  assertEqual "Example did not encode to expected value. Rotor double stepping." "ULMHJCJJC" result

cipheredExamplePlugboard :: Assertion
cipheredExamplePlugboard = do
  let r1 = rotorI 0
  let r2 = rotorI 0
  let r3 = rotorI 0
  let pb = createPlugboard [('A','O'),('R','Z'),('Q','B'),('U','E')]
  let sut = createEnigmaWithRotors [r3, r2, r1] reflectorB pb
  let plainText = replicate 8 'A'
  let result = evalState (encode plainText) sut
  assertEqual "Example did not encode to expected value. Rotor double stepping." "KOBSPCRC" result
  

singleCharacterNeverEncodesToItselfProperty :: AlphabetChar -> Property
singleCharacterNeverEncodesToItselfProperty (AlphabetChar c) = True ==> do
  let sut = enigmaAllRotor1
  let a = evalState (encode [c]) sut
  c /= head a

encodedStringShouldNotBeOriginalStringProperty :: AlphabetString -> Property
encodedStringShouldNotBeOriginalStringProperty (AlphabetString s) = True ==> do
  let sut = enigmaAllRotor1  
  let a = evalState (encode s) sut
  s /= a

encodedStringShouldDecodeToItselfProperty :: AlphabetString -> Property
encodedStringShouldDecodeToItselfProperty (AlphabetString s) = True ==> do
  let sut = enigmaAllRotor1
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
    , testCase "cipheredExample_3_Test" cipheredExample_3_Test
    , testCase "cipheredExample_4_Test" cipheredExample_4_Test
    , testCase "cipheredExample_5_Test" cipheredExample_5_Test
    , testCase "cipheredExample_double_stepping" cipheredExampleDoubleStepping
    , testCase "cipheredExample_plugboard" cipheredExamplePlugboard
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [
      QC.testProperty "singleCharacterNeverEncodesToItselfProperty" singleCharacterNeverEncodesToItselfProperty
    , QC.testProperty "encodedStringShouldNotBeOriginalStringProperty" encodedStringShouldNotBeOriginalStringProperty
    , QC.testProperty "encodedStringShouldDecodeToItselfProperty" encodedStringShouldDecodeToItselfProperty
  ]
