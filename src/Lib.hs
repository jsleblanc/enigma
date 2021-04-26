module Lib
    (
      Enigma,
      createEnigma
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

