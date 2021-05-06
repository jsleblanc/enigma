module Lib (
  Rotor,
  Enigma,
  createRotor,
  createEnigma,
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
      offset = 0
    }

type Offset = Int

data Rotor = Rotor {
  wiring :: Map.Map Int Int,
  offset :: Offset
} deriving Show

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

rotateRotor :: Rotor -> Rotor
rotateRotor r = do
  let o = offset r
  r {
    offset = addWithRollover o 1 26
  }

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
  let l = rotateRotor r
  e {
    rotors = l:rs
  }

type EnigmaState = State Enigma

encodeCharST :: Char -> EnigmaState Char
encodeCharST c = do
  enigmaState <- get
  let rotatedEnigma = doRotation enigmaState
  let encodedChar = cipher rotatedEnigma c
  put rotatedEnigma
  return encodedChar

encode :: String -> EnigmaState String
encode s = mapM encodeCharST s