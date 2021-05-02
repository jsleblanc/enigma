module Lib where

import Control.Monad (mapM)
import Control.Monad.State (State, get, put, modify, evalState, runState)
import Data.List
import qualified Data.Map as Map
import Debug.Trace

alphabetC :: String
alphabetC = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

letterToPosition :: Char -> Int
letterToPosition c =
  case elemIndex c alphabetC of
    Just i -> i
    Nothing -> error "Letter is not part of the alphabet!"

positionToLetter :: Int -> Char
positionToLetter i = alphabetC !! i

rotorFromStringMap :: String -> Rotor
rotorFromStringMap s = r
  where
    indexedString = zip [0..] s
    pairs = map (\(i,c) -> (i,letterToPosition c)) indexedString
    r = Rotor {
      wiring = Map.fromList pairs,
      offset = 0
    }

data Rotor = Rotor {
  wiring :: Map.Map Int Int,
  offset :: Int
} deriving Show

class LetterMapping a where
  lookupLetter :: a -> Int -> Char
  lookupPosition :: a -> Char -> Int

newtype Reflector = Reflector {
  reflectedWiring :: [Char]
} deriving Show

data Enigma = Enigma {
  rotors :: [Rotor],
  reflector :: Rotor
} deriving Show

-- Rotors are left-to-right order
createEnigma :: [String] -> String -> Enigma
createEnigma rs rf = Enigma {
  rotors = map rotorFromStringMap rs,
  reflector = rotorFromStringMap rf
}

substitute :: (LetterMapping a, LetterMapping b) => a -> Char -> b -> Char
substitute lm1 c lm2 = lookupLetter lm2 (lookupPosition lm1 c)

addWithRollover :: Int -> Int -> Int -> Int
addWithRollover value inc max = do
  let nv = value + inc
  if nv < max then nv else nv - max

cipherWithRotor :: Int -> Rotor -> Int
cipherWithRotor p r = do
  let o = offset r
  let np = addWithRollover p o 26
  case Map.lookup np (wiring r) of
    Just i -> i
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
  let fw = foldl cipherWithRotor p (reverse r)
  let reflected = cipherWithRotor fw (reflector e)
  let bw = foldl cipherWithRotor reflected r
  positionToLetter bw

doRotation :: Enigma -> Enigma
doRotation e = do
  let r = rotors e
  let l = rotateRotor (last r)
  let i = init r
  e {
    rotors = i ++ [l]
  }

encodeChar :: Char -> State Enigma Char
encodeChar c = do
  e <- get
  let re = doRotation e 
  let encoded = cipher re c
  put re
  get >>= traceShowM
  return encoded

encodeST :: String -> State Enigma String
encodeST s = do
  get >>= traceShowM
  encoded <- mapM encodeChar s
  return encoded

encode :: Enigma -> String -> String
encode e s = do
  evalState (encodeST s) e