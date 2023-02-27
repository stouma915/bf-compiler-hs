module ArgumentParse
    ( ParseResult
    , parseArgs
    ) where

import Data.List ( isPrefixOf )

data ParseResult = ShowHelp
                 | SourceFilePath String
                 | UnknownArg String

parseArgs :: [String] -> IO [ParseResult]
parseArgs [] = pure [ShowHelp]
parseArgs args = parseArgs' args []
  where
  parseArgs' :: [String] -> [ParseResult] -> IO [ParseResult]
  parseArgs' [] acc = pure acc
  parseArgs' strs acc = do
    let firstArg = head strs
    if isPrefixOf "-" firstArg then
      case firstArg of
        "--help" -> parseArgs' (drop 1 strs) (acc ++ [ShowHelp])
        "-h"     -> parseArgs' (drop 1 strs) (acc ++ [ShowHelp])
        other    -> parseArgs' (drop 1 strs) (acc ++ [UnknownArg other])
    else
      parseArgs' (drop 1 strs) (acc ++ [SourceFilePath firstArg])
