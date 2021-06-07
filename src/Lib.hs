module Lib (
  Rotor,
  Enigma,
  rotorI,
  rotorII,
  rotorIII,
  rotorIV,
  rotorV,
  reflectorA,
  reflectorB,
  reflectorC,
  createEnigmaWithRotors,
  createPlugboard,
  emptyPlugboard,
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
} -- deriving Show

instance Show Rotor where show r = show (offset r) ++ " (" ++ show (turnover r) ++ ")"

rotorI :: Int -> Rotor
rotorI startPosition = r {
  turnover = 16 --Q
} where r = createRotor "EKMFLGDQVZNTOWYHXUSPAIBRCJ" startPosition

rotorII :: Int -> Rotor
rotorII startPosition = r {
  turnover = 4 --E
} where r = createRotor "AJDKSIRUXBLHWTMCQGZNPYFVOE" startPosition

rotorIII :: Int -> Rotor
rotorIII startPosition = r {
  turnover = 21 -- V
} where r = createRotor "BDFHJLCPRTXVZNYEIWGAKMUSQO" startPosition

rotorIV :: Int -> Rotor
rotorIV startPosition = r {
  turnover = 9 -- J
} where r = createRotor "ESOVPZJAYQUIRHXLNFTGKDCMWB" startPosition

rotorV :: Int -> Rotor
rotorV startPosition = r {
  turnover = 25 -- Z
} where r = createRotor "VZBRGITYUPSDNHLXAWMJQOFECK" startPosition

reflectorA :: Rotor
reflectorA = createRotor "EJMZALYXVBWFCRQUONTSPIKHGD" 0

reflectorB :: Rotor
reflectorB = createRotor "YRUHQSLDPXNGOKMIEBFZCWVJAT" 0

reflectorC :: Rotor
reflectorC = createRotor "FVPJIAOYEDRZXWGCTKUQSBNMHL" 0


newtype Plugboard
  = Plugboard {letterSwaps :: Map.Map Int Int}
  deriving Show


swap :: (a,a) -> (a,a)
swap (x,y) = (y,x)

createPlugboard :: [(Char,Char)] -> Plugboard
createPlugboard [] = Plugboard {
  letterSwaps = Map.empty
}
createPlugboard pairs = Plugboard {
  letterSwaps = Map.fromList mappings
} where
  indexFst = map (letterToPosition . fst) pairs
  indexSnd = map (letterToPosition . snd) pairs
  indexedPairs = zip indexFst indexSnd
  swappedIndexedPairs = map swap indexedPairs
  mappings = indexedPairs ++ swappedIndexedPairs

emptyPlugboard :: Plugboard
emptyPlugboard = createPlugboard []

data Enigma = Enigma {
  rotors :: [Rotor],
  reflector :: Rotor,
  plugboard :: Plugboard
} deriving Show

createEnigmaWithRotors :: [Rotor] -> Rotor -> Plugboard -> Enigma
createEnigmaWithRotors rs rf pb = Enigma {
  rotors = rs,
  reflector = rf,
  plugboard = pb
}

addWithRollover :: Int -> Offset -> Int -> Int
addWithRollover value ofs maxv = do
  let nv = value + ofs
  if nv < 0 then maxv - 1 else
    if nv < maxv then nv else nv - maxv

subtractWithRollover :: Int -> Offset -> Int -> Int
subtractWithRollover value ofs maxv = do
  let nv = value - ofs
  if nv < 0 then maxv - negate nv else nv

cipherWithRotorRightToLeft :: Int -> Rotor -> Int
cipherWithRotorRightToLeft p r = do
  let o = offset r
  let np = addWithRollover p o 26  
  case Map.lookup np (wiring r) of
    Just i -> subtractWithRollover i o 26
    Nothing -> error ("Invalid rotor index " ++ show np ++ " offset " ++ show o)

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
    Nothing -> error ("Invalid rotor index " ++ show np ++ " offset " ++ show o)

shouldAdvanceNextRotor :: Rotor -> Bool
shouldAdvanceNextRotor r = do
  let o = offset r
  o == turnover r

rotateRotor :: Rotor -> (Bool, Rotor)
rotateRotor r = do
  let advance = shouldAdvanceNextRotor r
  let o = offset r
  let nr = r {
    offset = addWithRollover o 1 26
  }
  (advance, nr)

swapPlugboard :: Int -> Plugboard -> Int
swapPlugboard c pb = do
  let m = letterSwaps pb
  case Map.lookup c m of
    Just i -> i
    Nothing -> c

cipher :: Enigma -> Char -> Char
cipher e c = do
  let pb = plugboard e
  let r = rotors e  
  let p = letterToPosition c
  let pbSwap1 = swapPlugboard p pb
  let fw = foldl cipherWithRotorRightToLeft pbSwap1 r
  let reflected = cipherWithRotorRightToLeft fw (reflector e)
  let bw = foldl cipherWithRotorLeftToRight reflected (reverse r)
  let pbSwap2 = swapPlugboard bw pb
  positionToLetter pbSwap2

doRotationEnigma :: Enigma -> Enigma
doRotationEnigma e = do
  let ne = e {
    rotors = doRotationRotors (rotors e)
  }
  ne

doRotationRotors :: [Rotor] -> [Rotor]
doRotationRotors [] = []
doRotationRotors rotorsList = r2:rnext
  where
    (r1:rs) = rotorsList
    (propagate,r2) = rotateRotor r1 -- always advance first rotor
    rnext = propagateRotation propagate rs

propagateRotation :: Bool -> [Rotor] -> [Rotor]
propagateRotation _ [] = []
propagateRotation b rotorsList = r2:rnext
  where
    (r1:rs) = rotorsList
    advance = shouldAdvanceNextRotor r1
    (advanceNext,r2) = if b || advance then rotateRotor r1 else (False,r1)
    rnext = propagateRotation advanceNext rs

type EnigmaState = State Enigma

encodeCharST :: Char -> EnigmaState Char
encodeCharST c = do
  enigmaState <- get
  let rotatedEnigma = doRotationEnigma enigmaState
  let encodedChar = cipher rotatedEnigma c
  put rotatedEnigma
  -- get >>= traceShowM
  return encodedChar

encode :: String -> EnigmaState String
encode = mapM encodeCharST