module ArgumentParse
    ( RunMode (..)
    , ioParseArgsAndComputeRunMode
    , parseArgsAndComputeRunMode
    ) where

import Data.List ( isPrefixOf )

data ParseResult = ShowHelp
                 | SourceFilePath String
                 | UnknownArg String

isShowHelp :: ParseResult -> Bool
isShowHelp (ShowHelp) = True
isShowHelp _          = False
isSourceFilePath :: ParseResult -> Bool
isSourceFilePath (SourceFilePath _) = True
isSourceFilePath _                  = False
isUnknownArg :: ParseResult -> Bool
isUnknownArg (UnknownArg _) = True
isUnknownArg _              = False

getString :: ParseResult -> String
getString (ShowHelp)           = ""
getString (SourceFilePath str) = str
getString (UnknownArg     str) = str

data RunMode = ShowHelpMode
             | RunCompilerMode String
             | ShowUnknownArgErrorMode String

ioParseArgsAndComputeRunMode :: IO [String] -> IO RunMode
ioParseArgsAndComputeRunMode ioArgs = do
  args <- ioArgs
  pure $ parseArgsAndComputeRunMode args

parseArgsAndComputeRunMode :: [String] -> RunMode
parseArgsAndComputeRunMode = computeRunMode . parseArgs

computeRunMode :: [ParseResult] -> RunMode
computeRunMode [] = ShowHelpMode
computeRunMode args =
  if (length $ filter isUnknownArg args) /= 0 then
    ShowUnknownArgErrorMode $ getString (head $ filter isUnknownArg args)
  else if (length $ filter isShowHelp args) /= 0 then
    ShowHelpMode
  else if (length $ filter isSourceFilePath args) /= 0 then
    RunCompilerMode $ getString (last $ filter isSourceFilePath args)
  else
    ShowHelpMode

parseArgs :: [String] -> [ParseResult]
parseArgs = parseArgs' []
  where
  parseArgs' :: [ParseResult] -> [String] -> [ParseResult]
  parseArgs' acc [] = acc
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
