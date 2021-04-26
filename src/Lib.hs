module Lib
    (
      Enigma,
      createEnigma,
      encode
    ) where

import Data.List

class LetterMapping a where
  lookupLetter :: a -> Int -> Char
  lookupPosition :: a -> Char -> Int

newtype Rotor = Rotor {
  wiring :: [Char]
} deriving Show

instance LetterMapping Rotor where
  lookupLetter r p = (wiring r) !! p
  lookupPosition r l =
    case elemIndex l (wiring r) of
        Just p -> p
        Nothing -> error "fail"

rotate :: Rotor -> Rotor
rotate r = Rotor { wiring = tail w ++ [head w]}
  where w = wiring r


newtype Reflector = Reflector {
  reflectedWiring :: [Char]
} deriving Show

instance LetterMapping Reflector where
  lookupLetter r p = (reflectedWiring r) !! p
  lookupPosition r l =
    case elemIndex l (reflectedWiring r) of
        Just p -> p
        Nothing -> error "fail"

newtype Alphabet = Alphabet {
  reference :: [Char]
} deriving Show

instance LetterMapping Alphabet where
  lookupLetter r p = (reference r) !! p
  lookupPosition r l =
    case elemIndex l (reference r) of
        Just p -> p
        Nothing -> error "fail"

data Enigma = Enigma {
  rotors :: [Rotor],
  reflector :: Reflector,
  alphabet :: Alphabet
} deriving Show

createRotor :: String -> Rotor
createRotor s = Rotor {
    wiring = s
}

createReflector :: String -> Reflector
createReflector s = Reflector {
  reflectedWiring = s
}

createAlphabet :: String -> Alphabet
createAlphabet s = Alphabet {
  reference = s
}

createEnigma :: [String] -> String -> Enigma
createEnigma rs rf = Enigma {
  rotors = map createRotor rs,
  reflector = createReflector rf,
  alphabet = createAlphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
}

substitute :: (LetterMapping a, LetterMapping b) => a -> Char -> b -> Char
substitute lm1 c lm2 = lookupLetter lm2 (lookupPosition lm1 c)



encode :: Enigma -> Char -> IO ()
encode e c = do
  let a = alphabet e
  let r = rotors e
  let rf = reflector e
  let p1 = substitute a c (r !! 0)
  putChar p1
  let p2 = substitute a p1 (r !! 1)
  putChar p2
  let p3 = substitute a p2 (r !! 2)
  putChar p3
  let p4 = substitute a p3 rf
  putChar p4
  let p5 = substitute (r !! 2) p4 a
  putChar p5
  let p6 = substitute (r !! 1) p5 a
  putChar p6
  let p7 = substitute (r !! 0) p6 a
  putChar p7
