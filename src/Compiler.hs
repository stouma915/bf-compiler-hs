module Compiler
    ( CompileResult (..)
    , compileBf
    ) where

import Data.List ( intercalate )

import StringUtil

data CompileResult = CompileSuccess String
                   | SyntaxError Int Int String

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

increaseByte :: [String]
increaseByte =
  [ "    inc byte [edi]"
  ]

decreaseByte :: [String]
decreaseByte =
  [ "    dec byte [edi]"
  ]

increasePointer :: [String]
increasePointer =
  [ "    inc edi"
  ]

decreasePointer :: [String]
decreasePointer =
  [ "    dec edi"
  ]

executePrint :: [String]
executePrint =
  [ "    mov eax, 4"
  , "    mov ebx, 1"
  , "    mov ecx, edi"
  , "    mov edx, 1"
  , "    int 0x80"
  ]

loopPrefix :: Int -> [String]
loopPrefix labelNum =
  [ ""
  , "    jmp LB_" ++ (show $ labelNum + 1)
  , ""
  , "LB_" ++ (show $ labelNum + 1) ++ ":"
  , "    cmp byte [edi], 0"
  , "    je LB_" ++ (show $ labelNum + 2)
  , ""
  ]

loopSuffix :: Int -> [String]
loopSuffix labelNum =
  [ ""
  , "    jmp LB_" ++ (show labelNum)
  , ""
  , "LB_" ++ (show $ labelNum + 1) ++ ":"
  ]

assemblySuffix :: Int -> [String]
assemblySuffix labelNum =
  [ ""
  , "    jmp LB_" ++ (show $ labelNum + 1)
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

throwSyntaxError :: String -> Int -> String -> CompileResult
throwSyntaxError src index msg = do
  let line = getStrLine index src
  let ind = getStrIndex index src
  SyntaxError line ind msg

compileBf :: String -> CompileResult
compileBf src = compileBf' 0 0 False assemblyPrefix
  where
  compileBf' :: Int -> Int -> Bool -> [String] -> CompileResult
  compileBf' index labelNum isLooping acc =
    if index >= (length src) then
      if isLooping then
        throwSyntaxError src index "']' wasn't found."
      else
        CompileSuccess $ intercalate "\n" (acc ++ (assemblySuffix labelNum))
    else
      case src !! index of
        '+' ->
          compileBf' (index + 1) labelNum isLooping (acc ++ increaseByte)
        '-' ->
          compileBf' (index + 1) labelNum isLooping (acc ++ decreaseByte)
        '>' ->
          compileBf' (index + 1) labelNum isLooping (acc ++ increasePointer)
        '<' ->
          compileBf' (index + 1) labelNum isLooping (acc ++ decreasePointer)
        '.' ->
          compileBf' (index + 1) labelNum isLooping (acc ++ executePrint)
        '[' ->
          if isLooping then
            throwSyntaxError src index "Unexpected '['."
          else
            compileBf' (index + 1) (labelNum + 1) True (acc ++ (loopPrefix labelNum))
        ']' ->
          if not isLooping then
            throwSyntaxError src index "Unexpected ']'."
          else
            compileBf' (index + 1) (labelNum + 1) False (acc ++ (loopSuffix labelNum))
        _   ->
          compileBf' (index + 1) labelNum isLooping acc
