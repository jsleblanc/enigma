module Main where

import Lib
import Data.List
import qualified Data.Map as Map

e :: Enigma
-- 1,1,1
e = createEnigma ["EKMFLGDQVZNTOWYHXUSPAIBRCJ","EKMFLGDQVZNTOWYHXUSPAIBRCJ","EKMFLGDQVZNTOWYHXUSPAIBRCJ"] "YRUHQSLDPXNGOKMIEBFZCWVJAT"

r1 = rotorFromStringMap "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
r2 = rotorFromStringMap "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
rfl = rotorFromStringMap "YRUHQSLDPXNGOKMIEBFZCWVJAT"


main :: IO ()
main = do
  let r3 = rotorFromStringMap "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
  let r2 = rotorFromStringMap "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
  let o = 3
  let r1 = r3 {
    offset = o
  }
  putStrLn (show r1)
  putStrLn (show rfl)
  let c = 0
  let c1 = cipherWithRotorRightToLeft c r1
  let c2 = cipherWithRotorRightToLeft (c1-o) r2
  let c3 = cipherWithRotorRightToLeft (c2) r3
  let c4 = cipherWithRotorRightToLeft (c3) rfl
  let c5 = cipherWithRotorLeftToRight (c4) r3
  let c6 = cipherWithRotorLeftToRight (c5) r2
  let c7 = cipherWithRotorLeftToRight (c6) r1
  let c8 = c7-o
  putStrLn (show (c,c1,c2,c3,c4,c5,c6,c7,c8))
  putStrLn [(positionToLetter c8)]
  let res = encode e "AAA"
  print res


{-
First Encode 'A'
(0,4,11,19,25,9,25,9) - J

Second Encode 'A'
(0,9,25,9,23,16,7,

-}

{-
  ABCDEFGHIJKLMNOPQRSTUVWXYZ
  --------------------------
1 EKMFLGDQVZNTOWYHXUSPAIBRCJ
2 AJDKSIRUXBLHWTMCQGZNPYFVOE
3 BDFHJLCPRTXVZNYEIWGAKMUSQO
4 ESOVPZJAYQUIRHXLNFTGKDCMWB
5 VZBRGITYUPSDNHLXAWMJQOFECK
R YRUHQSLDPXNGOKMIEBFZCWVJAT
-}

{-
1,2,3 - A,A,A

AAAA -> UOTG

  ABCDEFGHIJKLMNOPQRSTUVWXYZ
  --------------------------
3 BDFHJLCPRTXVZNYEIWGAKMUSQO
2 AJDKSIRUXBLHWTMCQGZNPYFVOE
1 EKMFLGDQVZNTOWYHXUSPAIBRCJ
R YRUHQSLDPXNGOKMIEBFZCWVJAT

A -> B -> J -> Z -> T -> L -> K -> U

  ABCDEFGHIJKLMNOPQRSTUVWXYZ
  --------------------------
1 EKMFLGDQVZNTOWYHXUSPAIBRCJ
2 AJDKSIRUXBLHWTMCQGZNPYFVOE
3 BDFHJLCPRTXVZNYEIWGAKMUSQO
R YRUHQSLDPXNGOKMIEBFZCWVJAT

A -> E -> S -> G -> L -> F -> W -> N

-}

{-

const rotorTableColumns = ['name', 'label', 'wiring', 'turnovers']

/**
 * Table of rotor specs. The `rotorTableColumns` array defines how many columns
 * each row contains in which order. This table is converted to objects lazily.
 * The `name` column identifies each rotor and connects it to models.
 * The `wiring` column describes how characters are mapped relative to the
 * default alphabet.
 * @see  http://www.cryptomuseum.com/crypto/enigma/wiring.htm
 * @type {string[]}
 */
const rotorTable = [
  /* eslint-disable no-multi-spaces */

  // Entry rotor wired in alphabetical order
  'ETW-ABCDEF', 'Alphabet',   'abcdefghijklmnopqrstuvwxyz', '',

  // Entry rotor wired in order of keyboard
  'ETW-QWERTZ', 'Keyboard',   'jwulcmnohpqzyxiradkegvbtsf', '',

  // Enigma I, M3, M4
  'I',          'I',          'ekmflgdqvzntowyhxuspaibrcj', 'q',
  'II',         'II',         'ajdksiruxblhwtmcqgznpyfvoe', 'e',
  'III',        'III',        'bdfhjlcprtxvznyeiwgakmusqo', 'v',
  'IV',         'IV',         'esovpzjayquirhxlnftgkdcmwb', 'j',
  'V',          'V',          'vzbrgityupsdnhlxawmjqofeck', 'z',
  'VI',         'VI',         'jpgvoumfyqbenhzrdkasxlictw', 'zm',
  'VII',        'VII',        'nzjhgrcxmyswboufaivlpekqdt', 'zm',
  'VIII',       'VIII',       'fkqhtlxocbjspdzramewniuygv', 'zm',
  'beta',       'Beta',       'leyjvcnixwpbqmdrtakzgfuhos', '',
  'gamma',      'Gamma',      'fsokanuerhmbtiycwlqpzxvgjd', '',
  'UKW-A',      'UKW A',      'ejmzalyxvbwfcrquontspikhgd', '',
  'UKW-B',      'UKW B',      'yruhqsldpxngokmiebfzcwvjat', '',
  'UKW-C',      'UKW C',      'fvpjiaoyedrzxwgctkuqsbnmhl', '',
  'UKW-B-thin', 'UKW B thin', 'enkqauywjicopblmdxzvfthrgs', '',
  'UKW-C-thin', 'UKW C thin', 'rdobjntkvehmlfcwzaxgyipsuq', '',

  // Enigma I "Norenigma"
  'I-N',        'I',          'wtokasuyvrbxjhqcpzefmdinlg', 'q',
  'II-N',       'II',         'gjlpubswemctqvhxaofzdrkyni', 'e',
  'III-N',      'III',        'jwfmhnbpusdytixvzgrqlaoekc', 'v',
  'IV-N',       'IV',         'esovpzjayquirhxlnftgkdcmwb', 'j',
  'V-N',        'V',          'hejxqotzbvfdascilwpgynmurk', 'z',
  'UKW-N',      'UKW',        'mowjypuxndsraibfvlkzgqchet', '',

  // Enigma I "Sondermaschine"
  'I-S',        'I',          'veosirzujdqckgwypnxaflthmb', 'q',
  'II-S',       'II',         'uemoatqlshpkcyfwjzbgvxindr', 'e',
  'III-S',      'III',        'tzhxmbsipnurjfdkeqvcwglaoy', 'v',
  'UKW-S',      'UKW',        'ciagsndrbytpzfulvhekoqxwjm', '',

  // Enigma D
  'I-D',        'I',          'lpgszmhaeoqkvxrfybutnicjdw', 'y',
  'II-D',       'II',         'slvgbtfxjqohewirzyamkpcndu', 'e',
  'III-D',      'III',        'cjgdpshkturawzxfmynqobvlie', 'n',
  'UKW-COM',    'UKW',        'imetcgfraysqbzxwlhkdvupojn', '',

  // Enigma T "Tirpitz"
  'ETW-T',      'ETW',        'ilxrztkgjyamwvdufcpqeonshb', '',
  'I-T',        'I',          'kptyuelocvgrfqdanjmbswhzxi', 'wzekq',
  'II-T',       'II',         'uphzlweqmtdjxcaksoigvbyfnr', 'wzflr',
  'III-T',      'III',        'qudlyrfekonvzaxwhmgpjbsict', 'wzekq',
  'IV-T',       'IV',         'ciwtbkxnrespflydagvhquojzm', 'wzflr',
  'V-T',        'V',          'uaxgisnjbverdylfzwtpckohmq', 'ycfkr',
  'VI-T',       'VI',         'xfuzgalvhcnysewqtdmrbkpioj', 'xeimq',
  'VII-T',      'VII',        'bjvftxplnayozikwgdqeruchsm', 'ycfkr',
  'VIII-T',     'VIII',       'ymtpnzhwkodajxeluqvgcbisfr', 'xeimq',
  'UKW-T',      'UKW',        'gekpbtaumocniljdxzyfhwvqsr', '',

  // Swiss-K
  'I-KS',       'I',          'pezuohxscvfmtbglrinqjwaydk', 'y',
  'II-KS',      'II',         'zouesydkfwpciqxhmvblgnjrat', 'e',
  'III-KS',     'III',        'ehrvxgaobqusimzflynwktpdjc', 'n',

  // Railway Enigma "Rocket I"
  'I-KR',       'I',          'jgdqoxuscamifrvtpnewkblzyh', 'n',
  'II-KR',      'II',         'ntzpsfbokmwrcjdivlaeyuxhgq', 'e',
  'III-KR',     'III',        'jviubhtcdyakeqzposgxnrmwfl', 'y',
  'UKW-KR',     'UKW',        'qyhognecvpuztfdjaxwmkisrbl', '',

  // Enigma Zählwerk A-865
  'I-Z',        'I',          'lpgszmhaeoqkvxrfybutnicjdw', 'suvwzabcefgiklopq',
  'II-Z',       'II',         'slvgbtfxjqohewirzyamkpcndu', 'stvyzacdfghkmnq',
  'III-Z',      'III',        'cjgdpshkturawzxfmynqobvlie', 'uwxaefhkmnr',

  // Enigma Zählwerk G-111
  'I-G111',     'I',          'wlrhbqundkjczsexotmagyfpvi', 'suvwzabcefgiklopq',
  'II-G111',    'II',         'tfjqazwmhlcuixrdygoevbnskp', 'stvyzacdfghkmnq',
  'V-G111',     'V',          'qtpixwvdfrmusljohcanezkybg', 'swzfhmq',

  // Abwehr Enigma G-312
  'I-G312',     'I',          'dmtwsilruyqnkfejcazbpgxohv', 'suvwzabcefgiklopq',
  'II-G312',    'II',         'hqzgpjtmoblncifdyawveusrkx', 'stvyzacdfghkmnq',
  'III-G312',   'III',        'uqntlszfmrehdpxkibvygjcwoa', 'uwxaefhkmnr',
  'UKW-G312',   'UKW',        'rulqmzjsygocetkwdahnbxpvif', '',

  // Abwehr Enigma G-260
  'I-G260',     'I',          'rcspblkqaumhwytifzvgojnexd', 'suvwzabcefgiklopq',
  'II-G260',    'II',         'wcmibvpjxarosgndlzkeyhufqt', 'stvyzacdfghkmnq',
  'III-G260',   'III',        'fvdhzelsqmaxokyiwpgcbujtnr', 'uwxaefhkmnr'

  /* eslint-enable no-multi-spaces */
]

-}

