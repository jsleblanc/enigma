module Lib (
  Rotor,
  Enigma,
  rotor_I,
  rotor_II,
  rotor_III,
  reflector_A,
  reflector_B,
  createEnigmaWithRotors,
  encode
) where

import Control.Monad (mapM)
import Control.Monad.State (State, get, put, modify, evalState, runState)
import Data.List
import qualified Data.Map as Map
import Debug.Trace

alphabetC :: String
alphabetC = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

letterToPosition :: Char -> Int
letterToPosition c =
  case elemIndex c alphabetC of
    Just i -> i
    Nothing -> error "Letter is not part of the alphabet!"

positionToLetter :: Int -> Char
positionToLetter i = alphabetC !! i

createRotor :: String -> Offset -> Rotor
createRotor ws os = r { offset = os }
  where r = rotorFromStringMap ws

rotorFromStringMap :: String -> Rotor
rotorFromStringMap s = r
  where
    indexedString = zip [0..] s
    pairs = map (\(i,c) -> (i,letterToPosition c)) indexedString
    r = Rotor {
      wiring = Map.fromList pairs,
      offset = 0,
      turnover = 0
    }

type Offset = Int

data Rotor = Rotor {
  wiring :: Map.Map Int Int,
  offset :: Offset,
  turnover :: Int
} deriving Show

rotor_I :: Int -> Rotor
rotor_I startPosition = r {
  turnover = 16 --Q
} where r = createRotor "EKMFLGDQVZNTOWYHXUSPAIBRCJ" startPosition

rotor_II :: Int -> Rotor
rotor_II startPosition = r {
  turnover = 4 --E
} where r = createRotor "AJDKSIRUXBLHWTMCQGZNPYFVOE" startPosition

rotor_III :: Int -> Rotor
rotor_III startPosition = r {
  turnover = 21 -- V
} where r = createRotor "BDFHJLCPRTXVZNYEIWGAKMUSQO" startPosition

reflector_A :: Rotor
reflector_A = createRotor "EJMZALYXVBWFCRQUONTSPIKHGD" 0

reflector_B :: Rotor
reflector_B = createRotor "YRUHQSLDPXNGOKMIEBFZCWVJAT" 0


data Enigma = Enigma {
  rotors :: [Rotor],
  reflector :: Rotor
} deriving Show

-- Put Rotors in right-to-left order; rightmost position is 1, leftmost is 3 (or 4), reflector is in the left-most position
createEnigma :: [String] -> String -> Enigma
createEnigma rs rf =
  createEnigmaWithRotors rotors reflector
  where
    rotors = map rotorFromStringMap rs
    reflector = rotorFromStringMap rf

createEnigmaWithRotors :: [Rotor] -> Rotor -> Enigma
createEnigmaWithRotors rs rf = Enigma {
  rotors = rs,
  reflector = rf
}

addWithRollover :: Int -> Offset -> Int -> Int
addWithRollover value offset max = do
  let nv = value + offset
  if nv < 0 then max - 1 else
    if nv < max then nv else nv - max

subtractWithRollover :: Int -> Offset -> Int -> Int
subtractWithRollover value offset max = do
  let nv = value - offset
  if nv < 0 then max - (0 - nv) else nv

cipherWithRotorRightToLeft :: Int -> Rotor -> Int
cipherWithRotorRightToLeft p r = do
  let o = offset r
  let np = addWithRollover p o 26  
  case Map.lookup np (wiring r) of
    Just i -> subtractWithRollover i o 26
    Nothing -> error ("Invalid rotor index " ++ (show np) ++ " offset " ++ (show o))

keyFromValue :: Int -> Map.Map Int Int -> Maybe Int
keyFromValue value m = do
  let x = map fst $ filter ((== value) . snd) (Map.assocs m)
  case x of
    [key] -> Just key
    [] -> Nothing

cipherWithRotorLeftToRight :: Int -> Rotor -> Int
cipherWithRotorLeftToRight p r = do
  let o = offset r
  let np = addWithRollover p o 26  
  case keyFromValue np (wiring r) of
    Just i -> subtractWithRollover i o 26
    Nothing -> error ("Invalid rotor index " ++ (show np) ++ " offset " ++ (show o))

rotateRotor :: Rotor -> (Bool, Rotor)
rotateRotor r = do
  let o = offset r
  let nr = r {
    offset = addWithRollover o 1 26
  }
  let passedTurnover = (o - 1) == turnover r
  (passedTurnover, nr)

cipher :: Enigma -> Char -> Char
cipher e c = do
  let r = rotors e
  let p = letterToPosition c
  let fw = foldl cipherWithRotorRightToLeft p r
  let reflected = cipherWithRotorRightToLeft fw (reflector e)
  let bw = foldl cipherWithRotorLeftToRight reflected (reverse r)
  positionToLetter bw

doRotation :: Enigma -> Enigma
doRotation e = do
  let (r:rs) = rotors e
  let (b,l) = rotateRotor r
  let ne = e {
    rotors = l:rs
  }
  traceShow b $ ne

type EnigmaState = State Enigma

encodeCharST :: Char -> EnigmaState Char
encodeCharST c = do
  enigmaState <- get
  let rotatedEnigma = doRotation enigmaState
  let encodedChar = cipher rotatedEnigma c
  put rotatedEnigma
  get >>= traceShowM
  return encodedChar

encode :: String -> EnigmaState String
encode s = mapM encodeCharST s