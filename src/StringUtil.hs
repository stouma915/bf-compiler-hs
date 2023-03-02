module StringUtil
    ( splitWithSep
    , getStrLine
    , getStrIndex
    ) where

import Data.List ( elemIndex )

splitWithSep :: Char -> String -> [String]
splitWithSep sep str = splitWithSep' str []
  where
  splitWithSep' :: String -> [String] -> [String]
  splitWithSep' remain acc =
    case elemIndex sep remain of
      Just x ->
        splitWithSep' (drop (x + 1) remain) (acc ++ [take (x + 1) remain])
      Nothing ->
        acc ++ [remain]

getStrLine :: Int -> String -> Int
getStrLine index str =
  (length $ filter (== '\n') (take index str)) + 1

getStrIndex :: Int -> String -> Int
getStrIndex index str =
  length (last (splitWithSep '\n' (take index str)))
