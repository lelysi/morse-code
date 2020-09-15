module Lib where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tuple ( swap )
import Data.Maybe ( fromMaybe )

decodeStr :: [Char] -> [Char]
decodeStr = fromMaybe "" . decodeMorse

decodeMorse :: [Char] -> Maybe [Char]
decodeMorse str = mapM (`Map.lookup` morseMap) (words' str)

-- todo: make it better
words' :: [Char] -> [[Char]]
words' xs = helper xs [] [] False
    where
        helper "" result acc accN = result ++ [acc]
        helper (x:xs) res acc accN
            | x == ' ' && accN = helper xs (res ++ [[x]]) [] False
            | x == ' '         = helper xs (if null acc then res else res ++ [acc]) [] True
            | otherwise        = helper xs res (acc ++ [x]) False

morseMap :: Map [Char] Char
morseMap = Map.fromList $ map swap morseCodeList

morseDictionary :: Map Char [Char]
morseDictionary = Map.fromList morseCodeList

morseCodeList :: [(Char, [Char])]
morseCodeList =
  [(' ', " ")
  ,('a', ".-")
  ,('b', "-...")
  ,('c', "-.-.")
  ,('d', "-..")
  ,('e', ".")
  ,('f', "..-.")
  ,('g', "--.")
  ,('h', "....")
  ,('i', "..")
  ,('j', ".---")
  ,('k', "-.-")
  ,('l', ".-..")
  ,('m', "--")
  ,('n', "-.")
  ,('o', "---")
  ,('p', ".--.")
  ,('q', "--.-")
  ,('r', ".-.")
  ,('s', "...")
  ,('t', "-")
  ,('u', "..-")
  ,('v', "...-")
  ,('w', ".--")
  ,('x', "-..-")
  ,('y', "-.--")
  ,('z', "--..")
  ,('=', "-...-")
  ,('?', "..--..")
  ,('/', "-..-.")
  ,(',', "--..--")
  ,('.', ".-.-.-")
  ,(':', "---...")
  ,('\'', ".----.")
  ,('-', "-....-")
  ,('(', "-.--.")
  ,(')', "-.--.-")
  ,('0', "-----")
  ,('1', ".----")
  ,('2', "..---")
  ,('3', "...--")
  ,('4', "....-")
  ,('5', ".....")
  ,('6', "-....")
  ,('7', "--...")
  ,('8', "---..")
  ,('9', "----.")
  ,('@', ".--.-.")]