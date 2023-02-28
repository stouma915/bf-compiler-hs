module Main (main) where

import System.Environment

import ArgumentParse

version :: String
version = "1.0.0"

showUsage :: IO ()
showUsage = do
  progName <- getProgName
  putStrLn   "Usage:"
  putStrLn $ "   " ++ progName ++ " [OPTIONS] [SOURCE]"

showHelp :: IO ()
showHelp = do
  putStrLn $ "bf-compiler-hs v" ++ version
  putStrLn   ""
  showUsage
  putStrLn   ""
  putStrLn   "Options:"
  putStrLn   "   -h, --help    print help information."
  putStrLn   ""
  putStrLn   "Args:"
  putStrLn   "   <SOURCE>      Brainf**k source file."

showUnknownArgError :: String -> IO ()
showUnknownArgError arg = do
  putStrLn $ "Found argument '" ++ arg ++ "' which wasn't expected, or isn't valid in this context."
  putStrLn   ""
  showUsage
  putStrLn   ""
  putStrLn   "For more information try --help"

main :: IO ()
main = do
  runMode <- ioParseArgsAndComputeRunMode getArgs
  case runMode of
    (ShowHelpMode) ->
      showHelp
    (RunCompilerMode srcFilePath) ->
      putStrLn $ "RunCompiler " ++ srcFilePath
    (ShowUnknownArgErrorMode arg) ->
      showUnknownArgError arg
