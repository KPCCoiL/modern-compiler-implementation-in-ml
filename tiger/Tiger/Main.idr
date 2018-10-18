module Main

import Text.Lexer
import Tiger.Lexer

testLex : String -> IO ()
testLex s = do
  let (token, a, b, rest) = lexTiger s
  printLn $ map tok token
  printLn (a, b)
  putStrLn rest

main : IO ()
main = case !(readFile "/dev/stdin") of
            Right code => testLex code
            Left err => print err
