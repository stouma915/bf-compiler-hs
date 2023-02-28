module Main (main) where

import System.Environment

import ArgumentParse

main :: IO ()
main = do
  runMode <- ioParseArgsAndComputeRunMode getArgs
  case runMode of
    (ShowHelpMode)                -> putStrLn "ShowHelp"
    (RunCompilerMode srcFilePath) -> putStrLn $ "RunCompiler " ++ srcFilePath
    (ShowUnknownArgErrorMode arg) -> putStrLn $ "UnknownArg " ++ arg
