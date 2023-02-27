module ArgumentParse
    ( ParseResult
    , parseArgs
    ) where

import Data.List ( isPrefixOf )

data ParseResult = ShowHelp
                 | SourceFilePath String
                 | UnknownArg String

parseArgs :: IO [String] -> IO [ParseResult]
parseArgs ioArgs = ioArgs >>= parseArgs' []
  where
  parseArgs' :: [ParseResult] -> [String] -> IO [ParseResult]
  parseArgs' acc [] = pure acc
  parseArgs' acc strs = do
    let firstArg = head strs
    if isPrefixOf "-" firstArg then
      case firstArg of
        "--help" ->
          parseArgs' (acc ++ [ShowHelp]) (drop 1 strs)
        "-h" ->
          parseArgs' (acc ++ [ShowHelp]) (drop 1 strs)
        other ->
          parseArgs' (acc ++ [UnknownArg other]) (drop 1 strs)
    else
      parseArgs' (acc ++ [SourceFilePath firstArg]) (drop 1 strs)
