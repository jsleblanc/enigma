import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.Runners.Console
import Data.Monoid
import Control.Monad
-- import Utils
import Test.QuickCheck
import Lib

sut :: Enigma
sut = createEnigma ["BDFHJLCPRTXVZNYEIWGAKMUSQO","AJDKSIRUXBLHWTMCQGZNPYFVOE","EKMFLGDQVZNTOWYHXUSPAIBRCJ"] "YRUHQSLDPXNGOKMIEBFZCWVJAT"

singleCharacterTest :: Assertion
singleCharacterTest = do
  let c = 'A'
  let (a,_) = encode sut c
  let (b,_) = encode sut a
  assertEqual "Encoded character should decode to original starting character" c b

main :: IO ()
main = defaultMainWithOpts
  [
    testCase "singleCharacterTest" singleCharacterTest
  ]
  mempty

