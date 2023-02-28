module Compiler
    ( CompileResult (..)
    , compileBf
    ) where

import Data.List ( intercalate )

data CompileResult = CompileSuccess String
                   | SyntaxError String

assemblyPrefix :: [String]
assemblyPrefix =
  [ "section .text"
  , "    global _start"
  , ""
  , "_start:"
  , "    mov edi, buffer"
  , ""
  , "    jmp LB_0"
  , ""
  , "LB_0:"
  ]

assemblySuffix :: Int -> [String]
assemblySuffix labelNum =
  [ ""
  , "jmp LB_" ++ (show $ labelNum + 1)
  , ""
  , "LB_" ++ (show $ labelNum + 1) ++ ":"
  , "    mov eax, 1"
  , "    mov ebx, 0"
  , "    int 0x80"
  , ""
  , "section .bss"
  , "    buffer: resb 1000"
  , ""
  ]

compileBf :: String -> CompileResult
compileBf src = compileBf' 0 0 False assemblyPrefix
  where
  compileBf' :: Int -> Int -> Bool -> [String] -> CompileResult
  compileBf' index labelNum isLooping acc =
    if index >= (length src) then
      CompileSuccess $ intercalate "\n" (acc ++ (assemblySuffix labelNum))
    else
      compileBf' (index + 1) labelNum isLooping acc
    
